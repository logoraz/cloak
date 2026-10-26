;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:cloak/base/sessions
  (:use #:cl)
  (:local-nicknames (#:ext #:sb-ext))
  (:export #:find-sessions
           #:desktop-session
           #:desktop-session-name
           #:desktop-session-cmd)
  (:documentation "Finds the desktop sessions the greeter can start."))

(in-package #:cloak/base/sessions)


;;; Code:

(defstruct (desktop-session (:copier nil))
  "A session the user can pick. NAME is the menu label; CMD is the program
and its arguments, a vector of strings, as greetd's start_session wants."
  (name "" :read-only t)
  (cmd #() :read-only t))

(defun xdg-data-dirs ()
  "Return the directories to search for sessions: the entries of
XDG_DATA_DIRS, or the XDG default when it is unset or empty."
  (or (remove "" (uiop:split-string
                  (or (ext:posix-getenv "XDG_DATA_DIRS") "")
                  :separator ":")
              :test #'string=)
      (list "/usr/local/share" "/usr/share")))

(defun session-files (dir)
  "Return the .desktop files in the wayland-sessions directory under the
data directory DIR, sorted by name."
  (sort (directory (format nil "~A/wayland-sessions/*.desktop"
                           (string-right-trim "/" dir)))
        #'string< :key #'namestring))

(defun key-value (line)
  "Split LINE at its first = into a cons of the trimmed key and value, or
return nil if LINE has no =."
  (let ((split (position #\= line)))
    (when split
      (cons (string-trim " " (subseq line 0 split))
            (string-trim " " (subseq line (1+ split)))))))

(defun read-desktop-file (path)
  "Return the keys of the [Desktop Entry] group of the file at PATH, as an
alist of strings. Localized keys such as Name[fr] keep their full names.
Return nil, after a warning, if the file cannot be read."
  (handler-case
      (with-open-file (in path :external-format '(:utf-8 :replacement #\?))
        (let ((in-entry nil)
              (entries '()))
          (loop :for text := (read-line in nil)
                :while text
                :do (let ((line (string-trim '(#\Space #\Tab #\Return) text)))
                      (cond ((or (zerop (length line))
                                 (char= (char line 0) #\#)))
                            ((char= (char line 0) #\[)
                             (setf in-entry (string= line "[Desktop Entry]")))
                            ((and in-entry (key-value line))
                             (push (key-value line) entries)))))
          (nreverse entries)))
    (error (condition)
      (warn "sessions: cannot read ~A: ~A" path condition)
      nil)))

(defun expand-percent (arg)
  "Return ARG with each %% turned into %, or nil if ARG is a field code
such as %f, which is dropped."
  (if (and (= (length arg) 2)
           (char= (char arg 0) #\%)
           (alpha-char-p (char arg 1)))
      nil
      (with-output-to-string (out)
        (loop :with index := 0
              :while (< index (length arg))
              :do (write-char (char arg index) out)
                  (incf index (if (and (char= (char arg index) #\%)
                                       (< (1+ index) (length arg))
                                       (char= (char arg (1+ index)) #\%))
                                  2
                                  1))))))

(defun split-exec (string)
  "Split the Exec line STRING into a vector of arguments. Double quotes
group words, and inside them a backslash escapes the next character.
Field codes such as %f are dropped; %% becomes %."
  (let ((args '())
        (chars '())
        (quoted nil)
        (started nil)
        (index 0)
        (end (length string)))
    (flet ((add (char)
             (push char chars)
             (setf started t))
           (finish ()
             (when started
               (push (coerce (nreverse chars) 'string) args))
             (setf chars '()
                   started nil)))
      (loop :while (< index end)
            :do (let ((char (char string index)))
                  (cond ((and quoted (char= char #\\) (< (1+ index) end))
                         (incf index)
                         (add (char string index)))
                        ((char= char #\")
                         (setf quoted (not quoted)
                               started t))
                        ((and (not quoted) (member char '(#\Space #\Tab)))
                         (finish))
                        (t (add char))))
                (incf index))
      (finish))
    (coerce (remove nil (mapcar #'expand-percent (nreverse args))) 'vector)))

(defun entry->session (entries)
  "Return a DESKTOP-SESSION for ENTRIES, the alist from READ-DESKTOP-FILE,
or nil if the entry is hidden or has no Name or no Exec."
  (flet ((value (key)
           (cdr (assoc key entries :test #'string=))))
    (let ((name (value "Name"))
          (cmd (split-exec (or (value "Exec") ""))))
      (unless (or (equal (value "Hidden") "true")
                  (equal (value "NoDisplay") "true")
                  (null name)
                  (string= name "")
                  (zerop (length cmd)))
        (make-desktop-session :name name :cmd cmd)))))

(defun find-sessions (&optional (dirs (xdg-data-dirs)))
  "Return the sessions found in the wayland-sessions directory of each data
directory in DIRS. A session whose name was found in an earlier directory
is skipped, so earlier directories win."
  (let ((sessions '()))
    (dolist (dir dirs)
      (dolist (path (session-files dir))
        (let ((session (entry->session (read-desktop-file path))))
          (when (and session
                     (not (find (desktop-session-name session) sessions
                                :key #'desktop-session-name
                                :test #'string=)))
            (push session sessions)))))
    (nreverse sessions)))
