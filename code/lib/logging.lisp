;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:cloak/lib/logging
  (:use #:cl)
  (:local-nicknames (#:sbt #:sb-thread)
                    (#:psx #:sb-posix))
  (:export #:open-log
           #:close-log
           #:log-message)
  (:documentation "Log file for the greeter, with rotation."))

(in-package #:cloak/lib/logging)


;;; Code:

(defvar *log* nil
  "The open log stream, or nil, in which case messages go to the standard
error stream.")

(defvar *log-lock* (sbt:make-mutex :name "log")
  "Serializes writes, since worker threads log too.")

(defun rotated-name (path number)
  "Return the name of rotated log NUMBER of the log at PATH."
  (format nil "~A.~D" path number))

(defun rotate-log (path &optional (keep 5))
  "Rename the log at PATH to PATH.1, PATH.1 to PATH.2, and so on, keeping
KEEP rotated logs; the oldest is overwritten. Missing files are skipped."
  (loop :for number :from (1- keep) :downto 1
        :do (when (probe-file (rotated-name path number))
              (psx:rename (rotated-name path number)
                          (rotated-name path (1+ number)))))
  (when (probe-file path)
    (psx:rename path (rotated-name path 1))))

(defun close-log ()
  "Close the log file, if one is open. Messages then go to the standard
error stream."
  (sbt:with-mutex (*log-lock*)
    (when *log*
      (close *log*)
      (setf *log* nil))))

(defun open-log (path)
  "Rotate the log at PATH, then open a new one there. Return the stream.
If the log cannot be opened, warn and return nil; messages then go to the
standard error stream."
  (close-log)
  (sbt:with-mutex (*log-lock*)
    (setf *log*
          (handler-case
              (progn
                (rotate-log path)
                (open path :direction :output
                           :if-exists :append
                           :if-does-not-exist :create
                           :external-format :utf-8))
            (error (condition)
              (warn "log: cannot open ~A: ~A" path condition)
              nil)))))

(defun timestamp ()
  "Return the local time as YYYY-MM-DD HH:MM:SS."
  (multiple-value-bind (second minute hour day month year)
      (get-decoded-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour minute second)))

(defun log-message (control &rest args)
  "Write one line to the log: the time, then the message made by the format
CONTROL and ARGS."
  (sbt:with-mutex (*log-lock*)
    (let ((stream (or *log* *error-output*)))
      (format stream "~A ~?~%" (timestamp) control args)
      (finish-output stream))))
