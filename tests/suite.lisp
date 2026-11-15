(defpackage #:cloak/tests/suite
  (:use #:cl #:5am)
  (:import-from #:cloak/base/greetd-ipc
                #:read-frame
                #:write-frame)
  (:local-nicknames (#:sox #:sb-bsd-sockets)
                    (#:sbt #:sb-thread)
                    (#:psx #:sb-posix))
  (:export #:suite)
  (:export #:with-temp-dir
           #:write-text
           #:read-lines
           #:collect-warnings)
  (:export #:start-mock-cloak
           #:stop-mock-cloak
           #:mock-cloak-path)
  (:documentation "Root test suite for cloak."))

(in-package #:cloak/tests/suite)


(def-suite suite
  :description "All cloak unit tests.")


;;; Helpers

(defmacro with-temp-dir ((dir) &body body)
  "Run BODY with DIR bound to the name, ending in /, of a new empty
directory. Delete the directory afterwards."
  `(let ((,dir (format nil "~Acloak-test-~D-~36R/"
                       (namestring (uiop:temporary-directory))
                       (psx:getpid)
                       (random (expt 36 8) (make-random-state t)))))
     (ensure-directories-exist ,dir)
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree (pathname ,dir)
                                   :validate t
                                   :if-does-not-exist :ignore))))

(defun write-text (path text)
  "Write TEXT to the file at PATH, creating its directories. Return PATH."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (write-string text out))
  path)

(defun read-lines (path)
  "Return the lines of the file at PATH as a list of strings."
  (with-open-file (in path)
    (loop :for line := (read-line in nil)
          :while line
          :collect line)))

(defun collect-warnings (thunk)
  "Call THUNK and return its value and the warnings it signalled, as a
list of strings."
  (let ((warnings '()))
    (values (handler-bind ((warning (lambda (condition)
                                      (push (princ-to-string condition)
                                            warnings)
                                      (muffle-warning condition))))
              (funcall thunk))
            (nreverse warnings))))


;;; Mock Cloak

(defstruct (mock-cloak (:copier nil))
  "A fake greetd listening on a Unix socket, for tests."
  path server thread)

(defun serve-mock-cloak (server replies)
  "Accept one client on SERVER and answer one request per string in REPLIES.
Return the request strings read, in order. An error ends the service
quietly, since the test that caused it reports the failure."
  (let ((requests '()))
    (ignore-errors
      (let* ((client (sox:socket-accept server))
             (stream (sox:socket-make-stream client :input t :output t
                                              :element-type '(unsigned-byte 8)
                                              :buffering :full)))
        (unwind-protect
             (dolist (reply replies)
               (push (read-frame stream) requests)
               (write-frame stream reply))
          (close stream))))
    (nreverse requests)))

(defun start-mock-cloak (replies)
  "Start a fake greetd on a new Unix socket and return it as a MOCK-CLOAK.
It accepts one connection. For each string in REPLIES it reads one request
frame, then writes that string back as a reply frame. Call STOP-MOCK-CLOAK
when done."
  (let* ((path (format nil "~Acloak-mock-~36R.sock"
                       (uiop:temporary-directory)
                       (random (expt 36 8) (make-random-state t))))
         (server (make-instance 'sox:local-socket :type :stream)))
    (sox:socket-bind server path)
    (sox:socket-listen server 1)
    (make-mock-cloak
     :path path
     :server server
     :thread (sbt:make-thread
              (lambda () (serve-mock-cloak server replies))
              :name "mock-cloak"))))

(defun stop-mock-cloak (mock)
  "Shut down MOCK and return the request strings it received, in order.
Wait up to 2 seconds for the service to finish, then end it."
  (ignore-errors (sox:socket-close (mock-cloak-server mock)))
  (let ((requests (sbt:join-thread (mock-cloak-thread mock)
                                         :default nil :timeout 2)))
    (when (sbt:thread-alive-p (mock-cloak-thread mock))
      (sbt:terminate-thread (mock-cloak-thread mock)))
    (ignore-errors (delete-file (mock-cloak-path mock)))
    requests))
