;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:cloak/base/state
  (:use #:cl)
  (:export #:load-state
           #:save-state
           #:state-last-user
           #:state-last-session
           #:remember-login)
  (:documentation "Remembers the last user and session between runs."))

(in-package #:cloak/base/state)


;;; Code:

(defun valid-state-p (state)
  "True if STATE is a plist whose :last-user is a string or nil and whose
:sessions is an alist from user names to session names."
  (and (listp state)
       (null (cdr (last state)))
       (evenp (length state))
       (loop :for (key value) :on state :by #'cddr
             :always (case key
                       (:last-user (or (null value) (stringp value)))
                       (:sessions (and (listp value)
                                       (null (cdr (last value)))
                                       (every (lambda (pair)
                                                (and (consp pair)
                                                     (stringp (car pair))
                                                     (stringp (cdr pair))))
                                              value)))
                       (t t)))))

(defun read-state-file (path)
  "Return the first form of the file at PATH, read as data, or nil if the
file is missing or empty. Warn, and return nil, if it cannot be read."
  (handler-case
      (when (probe-file path)
        (with-open-file (in path :external-format '(:utf-8 :replacement #\?))
          (let ((*read-eval* nil)
                (*package* (find-package '#:cl-user)))
            (read in nil nil))))
    (error (condition)
      (warn "state: cannot read ~A: ~A" path condition)
      nil)))

(defun load-state (path)
  "Return the state saved in the file at PATH, a plist with :last-user and
:sessions. A missing file gives the empty state; a malformed one warns and
gives the empty state too."
  (let ((state (read-state-file path)))
    (cond ((null state) (list :last-user nil :sessions nil))
          ((valid-state-p state) state)
          (t (warn "state: ignoring malformed state in ~A" path)
             (list :last-user nil :sessions nil)))))

(defun save-state (path state)
  "Write STATE to the file at PATH. Return t, or nil after a warning if the
file cannot be written."
  (handler-case
      (with-open-file (out path :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
        (let ((*print-pretty* nil))
          (prin1 state out)
          (terpri out))
        t)
    (error (condition)
      (warn "state: cannot write ~A: ~A" path condition)
      nil)))

(defun state-last-user (state)
  "Return the name of the last user in STATE, or nil."
  (getf state :last-user))

(defun state-last-session (state user)
  "Return the name of the last session USER used in STATE, or nil."
  (cdr (assoc user (getf state :sessions) :test #'string=)))

(defun remember-login (state user session)
  "Return a new state like STATE, with USER as the last user and SESSION as
the last session of USER. STATE itself is not changed."
  (let ((others (remove user (getf state :sessions)
                        :key #'car :test #'string=)))
    (list :last-user user
          :sessions (acons user session others))))
