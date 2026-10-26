;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:cloak/tests/lib
  (:use #:cl #:5am)
  (:import-from #:cloak/tests/suite
                #:suite
                #:with-temp-dir
                #:write-text
                #:read-lines
                #:collect-warnings)
  (:import-from #:cloak/lib/jzon-message
                #:define-message
                #:decode-message)
  (:local-nicknames (#:jzon #:com.inuoe.jzon)
                    (#:sbt #:sb-thread)
                    (#:log #:cloak/lib/logging))
  (:export #:lib-suite)
  (:documentation "Tests for the lib layer."))

(in-package #:cloak/tests/lib)


(def-suite lib-suite
  :description "Tests for code/lib."
  :in suite)

(in-suite lib-suite)

;;; Code:

(define-message test-login "test_login" (username error-type))
(define-message test-ping "test_ping" ())
(define-message test-run "test_run" (cmd env))


;;; jzon-message


(test reader-fills-slots
      "JSON->NAME fills each slot from its JSON key."
      (let ((msg (json->test-login
                  "{\"username\":\"locutus\",\"error_type\":\"auth_error\"}")))
        (is (test-login-p msg))
        (is (string= "locutus" (test-login-username msg)))
        (is (string= "auth_error" (test-login-error-type msg)))))

(test reader-missing-key-is-nil
      "A key missing from the JSON leaves its slot NIL."
      (let ((msg (json->test-login "{\"username\":\"locutus\"}")))
        (is (string= "locutus" (test-login-username msg)))
        (is (null (test-login-error-type msg)))))

(test reader-ignores-unknown-keys
      "Keys that are not slots are ignored."
      (let ((msg (json->test-login "{\"username\":\"locutus\",\"extra\":1}")))
        (is (string= "locutus" (test-login-username msg)))))

(test reader-empty-message
      "A message with no slots reads from an object with no keys."
      (is (test-ping-p (json->test-ping "{}"))))

(test registry-holds-reader
      "DEFINE-MESSAGE registers its reader under the wire tag."
      (is (functionp
           (gethash "test_login"
                    cloak/lib/jzon-message::*message-readers*))))

(test decode-picks-struct-by-type
      "DECODE-MESSAGE chooses the struct from the type key."
      (let ((login (decode-message
                    "{\"type\":\"test_login\",\"username\":\"locutus\"}"))
            (ping (decode-message "{\"type\":\"test_ping\"}")))
        (is (test-login-p login))
        (is (string= "locutus" (test-login-username login)))
        (is (test-ping-p ping))))

(test decode-unknown-type-signals-error
      "An unregistered type signals an error."
      (signals error (decode-message "{\"type\":\"no_such_type\"}")))

(test decode-missing-type-signals-error
      "A message without a type key signals an error."
      (signals error (decode-message "{\"username\":\"locutus\"}")))


;;; jzon-message: writer

(test writer-writes-type-and-slots
      "NAME->JSON writes the type tag and each non-NIL slot."
      (let ((table (jzon:parse
                    (test-login->json
                     (make-test-login :username "locutus"
                                      :error-type "auth_error")))))
        (is (string= "test_login" (gethash "type" table)))
        (is (string= "locutus" (gethash "username" table)))
        (is (string= "auth_error" (gethash "error_type" table)))))

(test writer-omits-nil-slots
      "A NIL slot is left out of the JSON."
      (let ((table (jzon:parse
                    (test-login->json (make-test-login :username "locutus")))))
        (is (string= "locutus" (gethash "username" table)))
        (is (null (nth-value 1 (gethash "error_type" table))))
        (is (= 2 (hash-table-count table)))))

(test writer-empty-message
      "A message with no slots writes only the type tag."
      (let ((table (jzon:parse (test-ping->json (make-test-ping)))))
        (is (string= "test_ping" (gethash "type" table)))
        (is (= 1 (hash-table-count table)))))

(test writer-empty-vector-is-array
      "An empty vector is written as an empty JSON array, not omitted."
      (let* ((json (test-run->json (make-test-run :cmd #() :env #())))
             (table (jzon:parse json)))
        (is (equalp #() (gethash "cmd" table)))
        (is (equalp #() (gethash "env" table)))))

(test writer-vector-of-strings
      "A vector of strings is written as a JSON array of strings."
      (let ((table (jzon:parse
                    (test-run->json
                     (make-test-run :cmd #("sway" "--unsupported-gpu")
                                    :env #())))))
        (is (equalp #("sway" "--unsupported-gpu") (gethash "cmd" table)))))


;;; jzon-message: round trip

(test round-trip-parser
      "JSON->NAME reads back what NAME->JSON wrote."
      (let ((msg (make-test-login :username "locutus"
                                  :error-type "auth_error")))
        (is (equalp msg (json->test-login (test-login->json msg))))))

(test round-trip-omitted-slot
      "A NIL slot is NIL again after a round trip."
      (let ((msg (make-test-login :username "locutus")))
        (is (equalp msg (json->test-login (test-login->json msg))))))

(test round-trip-decode-message
      "DECODE-MESSAGE reads back each written message type."
      (dolist (msg (list (make-test-login :username "locutus")
                         (make-test-ping)
                         (make-test-run :cmd #("sway") :env #())))
        (is (equalp msg (decode-message
                         (etypecase msg
                           (test-login (test-login->json msg))
                           (test-ping (test-ping->json msg))
                           (test-run (test-run->json msg))))))))

;;; logging

(defmacro with-log ((path) &body body)
  "Run BODY with PATH bound to a log file name in a new directory. Close the
log and delete the directory afterwards."
  (let ((dir (gensym "DIR")))
    `(with-temp-dir (,dir)
       (let ((,path (format nil "~Alog" ,dir)))
         (unwind-protect (progn ,@body)
           (log:close-log))))))

(test log-message-writes-timestamped-line
      "LOG-MESSAGE writes the time, a space, then the formatted message."
      (with-log (path)
        (log:open-log path)
        (log:log-message "hello ~D" 42)
        (log:close-log)
        (let ((line (first (read-lines path))))
          (is (= 1 (length (read-lines path))))
          (is (digit-char-p (char line 0)))
          (is (char= #\- (char line 4)))
          (is (string= "hello 42" (subseq line 20))))))

(test open-log-rotates-the-old-log
      "Opening a log moves the previous one to log.1."
      (with-log (path)
        (log:open-log path)
        (log:log-message "first run")
        (log:close-log)
        (log:open-log path)
        (log:log-message "second run")
        (log:close-log)
        (is (search "first run" (first (read-lines (format nil "~A.1" path)))))
        (is (search "second run" (first (read-lines path))))))

(test open-log-keeps-five-rotated-logs
      "Only log.1 to log.5 are kept; log.1 is the newest rotated log."
      (with-log (path)
        (dotimes (run 8)
          (log:open-log path)
          (log:log-message "run ~D" run)
          (log:close-log))
        (dotimes (number 5)
          (is (probe-file (format nil "~A.~D" path (1+ number)))))
        (is (null (probe-file (format nil "~A.6" path))))
        (is (search "run 7" (first (read-lines path))))
        (is (search "run 6" (first (read-lines (format nil "~A.1" path)))))
        (is (search "run 2" (first (read-lines (format nil "~A.5" path)))))))

(test rotate-log-keep-count
      "ROTATE-LOG keeps as many rotated logs as it is told to."
      (with-log (path)
        (dotimes (run 4)
          (write-text path (format nil "run ~D~%" run))
          (log::rotate-log path 2))
        (is (null (probe-file path)))
        (is (equal '("run 3") (read-lines (format nil "~A.1" path))))
        (is (equal '("run 2") (read-lines (format nil "~A.2" path))))
        (is (null (probe-file (format nil "~A.3" path))))))

(test log-falls-back-to-error-output
      "An unopenable log warns once; messages then go to standard error."
      (let ((*error-output* (make-string-output-stream)))
        (multiple-value-bind (stream warnings)
            (collect-warnings
             (lambda () (log:open-log "/nonexistent/dir/log")))
          (is (null stream))
          (is (= 1 (length warnings))))
        (log:log-message "to stderr ~D" 1)
        (is (search "to stderr 1" (get-output-stream-string *error-output*)))))

(test close-log-stops-writing-to-the-file
      "After CLOSE-LOG messages no longer reach the file."
      (with-log (path)
        (log:open-log path)
        (log:close-log)
        (let ((*error-output* (make-string-output-stream)))
          (log:log-message "after close"))
        (is (null (read-lines path)))))

(test log-message-from-several-threads
      "Concurrent messages arrive as whole lines."
      (with-log (path)
        (log:open-log path)
        (let ((threads (loop :for id :below 4
                             :collect (let ((id id))
                                        (sbt:make-thread
                                         (lambda ()
                                           (dotimes (n 50)
                                             (log:log-message
                                              "thread ~D line ~D" id n))))))))
          (mapc #'sbt:join-thread threads))
        (log:close-log)
        (let ((lines (read-lines path)))
          (is (= 200 (length lines)))
          (is (every (lambda (line)
                       (and (char= #\- (char line 4))
                            (search " thread " line :start2 19)))
                     lines)))))
