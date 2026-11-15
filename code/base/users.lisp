(defpackage #:cloak/base/users
  (:use #:cl)
  (:export #:list-users
           #:user
           #:user-name
           #:user-uid)
  (:documentation "Lists the accounts that can log in."))

(in-package #:cloak/base/users)


;;; Code:

(defstruct (user (:copier nil))
  "An account that can log in: its login NAME and numeric UID."
  (name "" :read-only t)
  (uid 0 :read-only t))

(defun parse-passwd-line (line)
  "Return the name, uid and shell of the passwd LINE as three values, or
nil if LINE is not a well formed passwd entry."
  (let ((fields (uiop:split-string line :separator ":")))
    (when (= (length fields) 7)
      (let ((name (first fields))
            (uid (third fields)))
        (when (and (plusp (length name))
                   (plusp (length uid))
                   (every #'digit-char-p uid))
          (values name (parse-integer uid) (seventh fields)))))))

(defun login-user-p (uid shell)
  "True if an account with UID and SHELL is a person's login account: a UID
from 1000 up to, not including, 60000, and a shell that is not nologin or
false. An empty SHELL means the system default and is accepted."
  (let ((base (subseq shell (1+ (or (position #\/ shell :from-end t) -1)))))
    (and (<= 1000 uid)
         (< uid 60000)
         (not (member base '("nologin" "false") :test #'string=)))))

(defun list-users (&optional (path "/etc/passwd"))
  "Return a USER for each login account in the passwd file at PATH, in file
order. Return nil, after a warning, if the file cannot be read."
  (handler-case
      (with-open-file (in path :external-format '(:utf-8 :replacement #\?))
        (loop :for line := (read-line in nil)
              :while line
              :append (multiple-value-bind (name uid shell)
                          (parse-passwd-line line)
                        (when (and name (login-user-p uid shell))
                          (list (make-user :name name :uid uid))))))
    (error (condition)
      (warn "users: cannot read ~A: ~A" path condition)
      nil)))
