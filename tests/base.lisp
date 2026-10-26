;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:cloak/tests/base
  (:use #:cl #:5am
        #:cloak/base/greetd-ipc)
  (:import-from #:cloak/tests/suite
                #:suite
                #:with-temp-dir
                #:write-text
                #:read-lines
                #:collect-warnings
                #:start-mock-cloak
                #:stop-mock-cloak
                #:mock-cloak-path)
  (:import-from #:cloak/lib/jzon-message
                #:decode-message)
  (:local-nicknames (#:jzon #:com.inuoe.jzon)
                    (#:ext #:sb-ext)
                    (#:psx #:sb-posix)
                    (#:sbt #:sb-thread)
                    (#:lgn #:cloak/base/login)
                    (#:cfg #:cloak/base/config)
                    (#:ses #:cloak/base/sessions)
                    (#:usr #:cloak/base/users)
                    (#:ste #:cloak/base/state))
  (:export #:base-suite)
  (:documentation "Tests for the base layer."))

(in-package #:cloak/tests/base)


(def-suite base-suite
  :description "Tests for code/base."
  :in suite)

(in-suite base-suite)


;;; greetd-ipc: requests

(test request-create-session
      "CREATE-SESSION is written with its wire tag and username."
      (let ((table (jzon:parse
                    (create-session->json
                     (make-create-session :username "locutus")))))
        (is (string= "create_session" (gethash "type" table)))
        (is (string= "locutus" (gethash "username" table)))))

(test request-post-auth-response-with-answer
      "POST-AUTH-RESPONSE carries the answer when there is one."
      (let ((table (jzon:parse
                    (post-auth-response->json
                     (make-post-auth-response :response "secret")))))
        (is (string= "post_auth_message_response" (gethash "type" table)))
        (is (string= "secret" (gethash "response" table)))))

(test request-post-auth-response-without-answer
      "POST-AUTH-RESPONSE without an answer writes only the type."
      (let ((table (jzon:parse
                    (post-auth-response->json
                     (make-post-auth-response)))))
        (is (string= "post_auth_message_response" (gethash "type" table)))
        (is (= 1 (hash-table-count table)))))

(test request-start-session
      "START-SESSION writes cmd and env as arrays of strings."
      (let ((table (jzon:parse
                    (start-session->json
                     (make-start-session
                      :cmd #("sway")
                      :env #("XDG_SESSION_TYPE=wayland"))))))
        (is (string= "start_session" (gethash "type" table)))
        (is (equalp #("sway") (gethash "cmd" table)))
        (is (equalp #("XDG_SESSION_TYPE=wayland") (gethash "env" table)))))

(test request-cancel-session
      "CANCEL-SESSION writes only the type."
      (let ((table (jzon:parse
                    (cancel-session->json (make-cancel-session)))))
        (is (string= "cancel_session" (gethash "type" table)))
        (is (= 1 (hash-table-count table)))))

(test requests-round-trip
      "DECODE-MESSAGE reads back every request that was written."
      (dolist (case (list
                     (cons (make-create-session :username "locutus")
                           #'create-session->json)
                     (cons (make-post-auth-response :response "secret")
                           #'post-auth-response->json)
                     (cons (make-start-session :cmd #("sway") :env #())
                           #'start-session->json)
                     (cons (make-cancel-session)
                           #'cancel-session->json)))
        (is (equalp (car case)
                    (decode-message (funcall (cdr case) (car case)))))))


;;; greetd-ipc: responses

(test response-success
      "A success reply decodes to SUCCESS."
      (is (success-p (decode-message "{\"type\":\"success\"}"))))

(test response-error
      "An error reply keeps its error type and description."
      (let ((msg (decode-message
                  (concatenate
                   'string
                   "{\"type\":\"error\",\"error_type\":\"auth_error\","
                   "\"description\":\"pam_authenticate: AUTH_ERR\"}"))))
        (is (error-reply-p msg))
        (is (eq :auth-error (error-reply-error-type msg)))
        (is (string= "pam_authenticate: AUTH_ERR"
                     (error-reply-description msg)))))

(test response-auth-message-secret
      "A secret auth message keeps its type and prompt."
      (let ((msg (decode-message
                  (concatenate
                   'string
                   "{\"type\":\"auth_message\","
                   "\"auth_message_type\":\"secret\","
                   "\"auth_message\":\"Password:\"}"))))
        (is (auth-message-p msg))
        (is (eq :secret (auth-message-auth-message-type msg)))
        (is (string= "Password:" (auth-message-auth-message msg)))))

(test response-auth-message-info
      "An info auth message decodes the same way."
      (let ((msg (decode-message
                  (concatenate
                   'string
                   "{\"type\":\"auth_message\","
                   "\"auth_message_type\":\"info\","
                   "\"auth_message\":\"Fingerprint timed out\"}"))))
        (is (eq :info (auth-message-auth-message-type msg)))
        (is (string= "Fingerprint timed out"
                     (auth-message-auth-message msg)))))

(test response-unknown-enum-signals-error
      "An auth message type outside the protocol signals an error."
      (signals error
               (decode-message
                (concatenate
                 'string
                 "{\"type\":\"auth_message\","
                 "\"auth_message_type\":\"bogus\","
                 "\"auth_message\":\"Password:\"}"))))

;;; greetd-ipc: framing

(defun octet-vector (&rest octets)
  "Return OCTETS as a vector of (UNSIGNED-BYTE 8)."
  (make-array (length octets) :element-type '(unsigned-byte 8)
              :initial-contents octets))

(defun write-octets-to-file (path octets)
  "Write OCTETS to the file at PATH, replacing its contents."
  (with-open-file (out path :direction :output :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (write-sequence octets out)))

(defun file-octets (path)
  "Return the contents of the file at PATH as a vector of octets."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((vec (make-array (file-length in)
                           :element-type '(unsigned-byte 8))))
      (read-sequence vec in)
      vec)))

(defun written-octets (&rest strings)
  "Return the octets that WRITE-FRAME writes for each of STRINGS in turn."
  (uiop:with-temporary-file (:pathname path)
    (with-open-file (out path :direction :output :if-exists :supersede
                         :element-type '(unsigned-byte 8))
      (dolist (string strings)
        (write-frame out string)))
    (file-octets path)))

(defmacro with-input-octets ((stream octets) &body body)
  "Run BODY with STREAM open for reading on a binary file holding OCTETS."
  (let ((path (gensym "PATH")))
    `(uiop:with-temporary-file (:pathname ,path)
       (write-octets-to-file ,path ,octets)
       (with-open-file (,stream ,path :element-type '(unsigned-byte 8))
         ,@body))))

(test length-octets-values
      "LENGTH->OCTETS writes the least significant octet first."
      (is (equalp (octet-vector 0 0 0 0) (length->octets 0)))
      (is (equalp (octet-vector 1 0 0 0) (length->octets 1)))
      (is (equalp (octet-vector 255 0 0 0) (length->octets 255)))
      (is (equalp (octet-vector 0 1 0 0) (length->octets 256)))
      (is (equalp (octet-vector 64 66 15 0) (length->octets 1000000))))

(test length-octets-round-trip
      "OCTETS->LENGTH reverses LENGTH->OCTETS."
      (dolist (n '(0 1 255 256 65535 1000000 4294967295))
        (is (= n (octets->length (length->octets n))))))

(test length-octets-errors
      "Out of range lengths and wrong sized vectors signal errors."
      (signals error (length->octets 4294967296))
      (signals error (length->octets -1))
      (signals error (octets->length (octet-vector 1 2 3))))

(test read-octets-reads-count
      "READ-OCTETS reads exactly COUNT octets and leaves the rest."
      (with-input-octets (in (octet-vector 1 2 3 4 5))
        (is (equalp (octet-vector 1 2 3) (read-octets in 3)))
        (is (equalp (octet-vector 4 5) (read-octets in 2)))))

(test read-octets-short-signals-eof
      "READ-OCTETS signals END-OF-FILE when the stream ends early."
      (with-input-octets (in (octet-vector 1 2 3))
        (signals end-of-file (read-octets in 4))))

(test write-frame-header-and-payload
      "WRITE-FRAME writes the 4-octet length, then the JSON octets."
      (let ((octets (written-octets "{}")))
        (is (equalp (octet-vector 2 0 0 0 123 125) octets))))

(test write-frame-counts-octets-not-characters
      "The header counts UTF-8 octets, so an accented letter counts 2."
      (let ((octets (written-octets "é")))
        (is (equalp (octet-vector 2 0 0 0 195 169) octets))))

(test frame-round-trip
      "READ-FRAME returns the string that WRITE-FRAME wrote."
      (dolist (string '("{}" "{\"type\":\"success\"}" "héllo"))
        (with-input-octets (in (written-octets string))
          (is (string= string (read-frame in))))))

(test frame-two-in-a-row
      "READ-FRAME reads consecutive frames from one stream."
      (with-input-octets (in (written-octets "first" "second"))
        (is (string= "first" (read-frame in)))
        (is (string= "second" (read-frame in)))))

(test read-frame-truncated-signals-eof
      "READ-FRAME signals END-OF-FILE inside a frame, or on no data."
      (with-input-octets (in (octet-vector 10 0 0 0 1 2 3))
        (signals end-of-file (read-frame in)))
      (with-input-octets (in (octet-vector))
        (signals end-of-file (read-frame in))))


;;; greetd-ipc: transport

(defun run-exchanges (replies requests)
  "Exchange each of REQUESTS with a mock greetd that sends REPLIES.
Return two values: the responses received and the request strings the
mock saw, both in order."
  (let ((mock (start-mock-cloak replies))
        (stream nil)
        (responses '())
        (seen '()))
    (unwind-protect
         (progn
           (setf stream (connect-greetd (mock-cloak-path mock)))
           (dolist (request requests)
             (push (exchange stream request) responses)))
      (when stream (close stream))
      (setf seen (stop-mock-cloak mock)))
    (values (nreverse responses) seen)))

(test exchange-success
      "A success reply decodes, and the request reaches the server intact."
      (let ((request (make-create-session :username "locutus")))
        (multiple-value-bind (responses seen)
            (run-exchanges '("{\"type\":\"success\"}") (list request))
          (is (= 1 (length responses)))
          (is (success-p (first responses)))
          (is (equal (list (create-session->json request)) seen)))))

(test exchange-auth-message
      "An auth_message reply decodes with a keyword type."
      (let ((reply (concatenate 'string
                                "{\"type\":\"auth_message\","
                                "\"auth_message_type\":\"secret\","
                                "\"auth_message\":\"Password:\"}")))
        (let ((response (first (run-exchanges
                                (list reply)
                                (list (make-create-session
                                       :username "locutus"))))))
          (is (auth-message-p response))
          (is (eq :secret (auth-message-auth-message-type response)))
          (is (string= "Password:" (auth-message-auth-message response))))))

(test exchange-error-reply
      "An error reply decodes with a keyword error type and description."
      (let ((reply (concatenate 'string
                                "{\"type\":\"error\","
                                "\"error_type\":\"auth_error\","
                                "\"description\":\"bad password\"}")))
        (let ((response (first (run-exchanges
                                (list reply)
                                (list (make-post-auth-response
                                       :response "wrong"))))))
          (is (error-reply-p response))
          (is (eq :auth-error (error-reply-error-type response)))
          (is (string= "bad password" (error-reply-description response))))))

(test exchange-login-flow
      "Three exchanges on one connection arrive in order."
      (let* ((requests (list (make-create-session :username "locutus")
                             (make-post-auth-response :response "secret")
                             (make-start-session :cmd #("sway") :env #())))
             (replies (list (concatenate 'string
                                         "{\"type\":\"auth_message\","
                                         "\"auth_message_type\":\"secret\","
                                         "\"auth_message\":\"Password:\"}")
                            "{\"type\":\"success\"}"
                            "{\"type\":\"success\"}")))
        (multiple-value-bind (responses seen)
            (run-exchanges replies requests)
          (is (= 3 (length responses)))
          (is (auth-message-p (first responses)))
          (is (success-p (second responses)))
          (is (success-p (third responses)))
          (is (equal (mapcar #'request->json requests) seen)))))

(test send-then-receive
      "SEND-REQUEST and RECEIVE-RESPONSE work as separate steps."
      (let ((mock (start-mock-cloak '("{\"type\":\"success\"}")))
            (stream nil))
        (unwind-protect
             (progn
               (setf stream (connect-greetd (mock-cloak-path mock)))
               (send-request stream (make-cancel-session))
               (is (success-p (receive-response stream))))
          (when stream (close stream))
          (stop-mock-cloak mock))))

(test receive-response-closed-connection
      "A closed connection makes RECEIVE-RESPONSE signal EOF."
      (let ((mock (start-mock-cloak '()))
            (stream nil))
        (unwind-protect
             (progn
               (setf stream (connect-greetd (mock-cloak-path mock)))
               (signals end-of-file (receive-response stream)))
          (when stream (close stream))
          (stop-mock-cloak mock))))

(test exchange-rejects-unknown-request
      "A non-request object signals a type error before any I/O."
      (signals type-error (exchange nil 42)))

(test connect-greetd-missing-socket
      "Connecting to a path with no socket signals an error."
      (signals error
               (connect-greetd "/nonexistent/cloak-no-such.sock")))

(defmacro with-env ((name value) &body body)
  "Run BODY with environment variable NAME set to VALUE, or unset if
VALUE is NIL. Restore the previous setting afterwards."
  (let ((var (gensym "NAME"))
        (new (gensym "NEW"))
        (old (gensym "OLD")))
    `(let* ((,var ,name)
            (,new ,value)
            (,old (ext:posix-getenv ,var)))
       (unwind-protect
            (progn
              (if ,new
                  (psx:setenv ,var ,new 1)
                  (psx:unsetenv ,var))
              ,@body)
         (if ,old
             (psx:setenv ,var ,old 1)
             (psx:unsetenv ,var))))))

(test greetd-socket-path-reads-environment
      "GREETD-SOCKET-PATH returns the value of GREETD_SOCK."
      (with-env ("GREETD_SOCK" "/run/greetd.sock")
        (is (string= "/run/greetd.sock" (greetd-socket-path)))))

(test greetd-socket-path-unset-signals-error
      "GREETD-SOCKET-PATH signals an error when GREETD_SOCK is unset."
      (with-env ("GREETD_SOCK" nil)
        (signals error (greetd-socket-path))))


;;; login

(defun run-login (replies thunk)
  "Run THUNK with a LOGIN connected to a mock greetd that sends REPLIES.
THUNK receives the login and a function; calling that function with N
waits up to 2 seconds for N events, then returns every event so far.
Return the request strings the mock received, in order."
  (let ((mock (start-mock-cloak replies))
        (lock (sbt:make-mutex))
        (events '())
        (login nil)
        (seen '()))
    (flet ((callback (event)
             (sbt:with-mutex (lock)
               (setf events (append events (list event)))))
           (await (count)
             (loop repeat 200
                   until (sbt:with-mutex (lock) (>= (length events) count))
                   do (sleep 0.01))
             (sbt:with-mutex (lock) (copy-list events))))
      (unwind-protect
           (with-env ("GREETD_SOCK" (mock-cloak-path mock))
             (setf login (lgn:make-login #'callback))
             (funcall thunk login #'await))
        (when login (close (lgn::login-stream login) :abort t))
        (setf seen (stop-mock-cloak mock))))
    seen))

(defparameter *secret-prompt*
  (concatenate 'string
               "{\"type\":\"auth_message\","
               "\"auth_message_type\":\"secret\","
               "\"auth_message\":\"Password:\"}")
  "A greetd reply asking for a secret.")

(defparameter *success* "{\"type\":\"success\"}"
  "A greetd success reply.")

(test login-password-flow
      "A password login emits :prompt, :ready and :started in order."
      (let ((seen (run-login
                   (list *secret-prompt* *success* *success*)
                   (lambda (login await)
                     (lgn:begin-login login "locutus")
                     (is (equal '((:prompt :secret "Password:"))
                                (funcall await 1)))
                     (lgn:answer-prompt login "secret")
                     (is (equal '((:prompt :secret "Password:") (:ready))
                                (funcall await 2)))
                     (lgn:launch login #("sway") #())
                     (is (equal '((:prompt :secret "Password:")
                                  (:ready)
                                  (:started))
                                (funcall await 3)))))))
        (is (equal (list (create-session->json
                          (make-create-session :username "locutus"))
                         (post-auth-response->json
                          (make-post-auth-response :response "secret"))
                         (start-session->json
                          (make-start-session :cmd #("sway") :env #())))
                   seen))))

(test login-info-message-continues
      "An info message is reported, then answered with no response text."
      (let ((seen (run-login
                   (list (concatenate
                          'string
                          "{\"type\":\"auth_message\","
                          "\"auth_message_type\":\"info\","
                          "\"auth_message\":\"Touch the sensor\"}")
                         *secret-prompt*)
                   (lambda (login await)
                     (lgn:begin-login login "locutus")
                     (is (equal '((:message :info "Touch the sensor")
                                  (:prompt :secret "Password:"))
                                (funcall await 2)))))))
        (is (equal (list (create-session->json
                          (make-create-session :username "locutus"))
                         (post-auth-response->json
                          (make-post-auth-response)))
                   seen))))

(test login-auth-failure-cancels
      "An auth error is reported, the session is cancelled, state is :idle."
      (let ((seen (run-login
                   (list (concatenate
                          'string
                          "{\"type\":\"error\","
                          "\"error_type\":\"auth_error\","
                          "\"description\":\"bad password\"}")
                         *success*)
                   (lambda (login await)
                     (lgn:begin-login login "locutus")
                     (is (equal '((:auth-failed "bad password"))
                                (funcall await 1)))
                     (is (eq :idle (lgn::login-state login)))))))
        (is (equal (list (create-session->json
                          (make-create-session :username "locutus"))
                         (cancel-session->json (make-cancel-session)))
                   seen))))

(test login-connection-error
      "A connection that closes mid-request emits :error and resets state."
      (run-login
       '()
       (lambda (login await)
         (lgn:begin-login login "locutus")
         (is (eq :error (first (first (funcall await 1)))))
         (is (eq :idle (lgn::login-state login))))))

(test login-cancel
      "CANCEL-LOGIN returns to :idle and emits nothing for the reply."
      (let ((seen (run-login
                   (list *secret-prompt* *success*)
                   (lambda (login await)
                     (lgn:begin-login login "locutus")
                     (funcall await 1)
                     (sbt:join-thread (lgn:cancel-login login))
                     (is (eq :idle (lgn::login-state login)))
                     (is (equal '((:prompt :secret "Password:"))
                                (funcall await 0)))))))
        (is (equal (list (create-session->json
                          (make-create-session :username "locutus"))
                         (cancel-session->json (make-cancel-session)))
                   seen))))

(test login-wrong-order-signals-error
      "Calls made in the wrong state signal an error and send nothing."
      (let ((seen (run-login
                   '()
                   (lambda (login await)
                     (declare (ignore await))
                     (signals error (lgn:answer-prompt login "secret"))
                     (signals error (lgn:launch login #("sway") #()))))))
        (is (null seen))))


;;; config

(defun load-text (text)
  "Load a config file holding TEXT. Return the config and the warnings."
  (uiop:with-temporary-file (:stream out :pathname path)
    (write-string text out)
    (finish-output out)
    (collect-warnings (lambda () (cfg:load-config path)))))

(test config-empty-file-gives-defaults
      "An empty config gives every default and no warnings."
      (multiple-value-bind (config warnings) (load-text "()")
        (is (null warnings))
        (is (= 0.85 (cfg:config-ref config :theme :alpha)))
        (is (string= "#2e3440" (cfg:config-ref config :theme :background)))
        (is (string= "Welcome" (cfg:config-ref config :greeting)))
        (is (equal '("loginctl" "reboot")
                   (cfg:config-ref config :commands :reboot)))))

(test config-user-values-win
      "User values replace defaults; other settings keep theirs."
      (multiple-value-bind (config warnings)
          (load-text "(:theme (:alpha 0.5 :wallpaper (:fit :contain))
                       :greeting \"Hello locutus\")")
        (is (null warnings))
        (is (= 0.5 (cfg:config-ref config :theme :alpha)))
        (is (eq :contain (cfg:config-ref config :theme :wallpaper :fit)))
        (is (string= "Hello locutus" (cfg:config-ref config :greeting)))
        (is (string= "#2e3440" (cfg:config-ref config :theme :background)))))

(test config-invalid-value-uses-default
      "A value of the wrong type warns and takes the default."
      (multiple-value-bind (config warnings)
          (load-text "(:theme (:alpha \"high\" :cursor 5))")
        (is (= 2 (length warnings)))
        (is (search ":theme :alpha" (first warnings)))
        (is (search ":theme :cursor" (second warnings)))
        (is (= 0.85 (cfg:config-ref config :theme :alpha)))
        (is (string= "Bibata-Modern-Classic"
                     (cfg:config-ref config :theme :cursor)))))

(test config-number-range
      "A number must lie within :min and :max, ends included."
      (dolist (ok '("0" "1" "0.5"))
        (multiple-value-bind (config warnings)
            (load-text (format nil "(:theme (:alpha ~A))" ok))
          (is (null warnings))
          (is (= (read-from-string ok)
                 (cfg:config-ref config :theme :alpha)))))
      (dolist (bad '("-0.1" "1.1"))
        (multiple-value-bind (config warnings)
            (load-text (format nil "(:theme (:alpha ~A))" bad))
          (is (= 1 (length warnings)))
          (is (= 0.85 (cfg:config-ref config :theme :alpha))))))

(test config-invalid-enum-uses-default
      "An enum value outside :values warns and takes the default."
      (multiple-value-bind (config warnings)
          (load-text "(:layout (:position :top))")
        (is (= 1 (length warnings)))
        (is (eq :center (cfg:config-ref config :layout :position)))))

(test config-command-must-be-list-of-strings
      "A command is a list of strings; anything else takes the default."
      (multiple-value-bind (config warnings)
          (load-text "(:commands (:reboot (\"systemctl\" \"reboot\")))")
        (is (null warnings))
        (is (equal '("systemctl" "reboot")
                   (cfg:config-ref config :commands :reboot))))
      (multiple-value-bind (config warnings)
          (load-text "(:commands (:reboot \"reboot\" :poweroff (\"a\" 1)))")
        (is (= 2 (length warnings)))
        (is (equal '("loginctl" "reboot")
                   (cfg:config-ref config :commands :reboot)))
        (is (equal '("loginctl" "poweroff")
                   (cfg:config-ref config :commands :poweroff)))))

(test config-unknown-key-warns-and-is-ignored
      "Unknown keys warn at every level and are left out of the result."
      (multiple-value-bind (config warnings)
          (load-text "(:bogus 1 :theme (:nope 2))")
        (is (= 2 (length warnings)))
        (is (search ":bogus" (first warnings)))
        (is (search ":theme :nope" (second warnings)))
        (signals error (cfg:config-ref config :bogus))
        (signals error (cfg:config-ref config :theme :nope))))

(test config-group-must-be-key-value-pairs
      "A group that is not key and value pairs warns and gives defaults."
      (multiple-value-bind (config warnings) (load-text "(:theme 7)")
        (is (= 1 (length warnings)))
        (is (search ":theme" (first warnings)))
        (is (= 0.85 (cfg:config-ref config :theme :alpha)))))

(test config-top-level-must-be-key-value-pairs
      "A file that is not key and value pairs gives defaults."
      (multiple-value-bind (config warnings) (load-text "(1 2 3)")
        (is (= 1 (length warnings)))
        (is (string= "Welcome" (cfg:config-ref config :greeting)))))

(test config-missing-file-gives-defaults
      "A missing file warns once and gives defaults."
      (multiple-value-bind (config warnings)
          (collect-warnings
           (lambda () (cfg:load-config "/nonexistent/cloak.sexp")))
        (is (= 1 (length warnings)))
        (is (string= "Welcome" (cfg:config-ref config :greeting)))))

(test config-bad-syntax-gives-defaults
      "An unfinished form warns once and gives defaults."
      (multiple-value-bind (config warnings) (load-text "(:greeting")
        (is (= 1 (length warnings)))
        (is (string= "Welcome" (cfg:config-ref config :greeting)))))

(test config-does-not-evaluate
      "Reading never evaluates, so #. is refused."
      (multiple-value-bind (config warnings)
          (load-text "(:greeting #.(+ 1 2))")
        (is (= 1 (length warnings)))
        (is (string= "Welcome" (cfg:config-ref config :greeting)))))

(test config-ignores-forms-after-the-first
      "Only the first form is used; later forms warn."
      (multiple-value-bind (config warnings)
          (load-text "(:greeting \"a\") (:greeting \"b\")")
        (is (= 1 (length warnings)))
        (is (string= "a" (cfg:config-ref config :greeting)))))

(test config-ref-unknown-key-signals-error
      "CONFIG-REF signals an error for a key the schema lacks."
      (let ((config (load-text "()")))
        (signals error (cfg:config-ref config :theme :alphaa))
        (signals error (cfg:config-ref config :nothing))))

(test config-defaults-are-not-shared
      "Changing a returned default does not change later loads."
      (let ((one (load-text "()")))
        (setf (first (cfg:config-ref one :commands :reboot)) "changed")
        (is (equal '("loginctl" "reboot")
                   (cfg:config-ref (load-text "()") :commands :reboot)))))

(test config-clock-defaults
      "The clock defaults to 12-hour, light blue and 48px."
      (multiple-value-bind (config warnings) (load-text "()")
        (is (null warnings))
        (is (string= "%I:%M %p" (cfg:config-ref config :clock :format)))
        (is (string= "#81a1c1" (cfg:config-ref config :clock :color)))
        (is (string= "48px" (cfg:config-ref config :clock :size)))
        (is (string= "#e5e9f0" (cfg:config-ref config :theme :foreground)))))

(test config-clock-values-win
      "User clock values replace the defaults one by one."
      (multiple-value-bind (config warnings)
          (load-text "(:clock (:format \"%H:%M\" :size \"64px\"))")
        (is (null warnings))
        (is (string= "%H:%M" (cfg:config-ref config :clock :format)))
        (is (string= "64px" (cfg:config-ref config :clock :size)))
        (is (string= "#81a1c1" (cfg:config-ref config :clock :color)))))

(test config-clock-invalid-color-uses-default
      "A clock color that is not a string warns and takes the default."
      (multiple-value-bind (config warnings)
          (load-text "(:clock (:color 5))")
        (is (= 1 (length warnings)))
        (is (string= "#81a1c1" (cfg:config-ref config :clock :color)))))

;;; sessions

(defun desktop-entry (name exec &rest extra)
  "Return the text of a .desktop file with NAME, EXEC and EXTRA lines."
  (format nil "[Desktop Entry]~%Name=~A~%Exec=~A~%~{~A~%~}" name exec extra))

(defun session-names (sessions)
  "Return the names of SESSIONS."
  (mapcar #'ses:desktop-session-name sessions))

(defun session-cmd (session)
  "Return the command of SESSION as a list of strings."
  (coerce (ses:desktop-session-cmd session) 'list))

(defun exec-list (string)
  "Return the arguments SPLIT-EXEC finds in STRING, as a list."
  (coerce (ses::split-exec string) 'list))

(test split-exec-plain-words
      "SPLIT-EXEC splits on blanks."
      (is (equal '("sway") (exec-list "sway")))
      (is (equal '("sway" "-d" "--x") (exec-list "  sway   -d --x "))))

(test split-exec-quotes
      "Quotes group words, a backslash escapes inside them."
      (is (equal '("env" "my prog" "--a=b c")
                 (exec-list "env \"my prog\" --a=\"b c\"")))
      (is (equal '("q\"x") (exec-list "\"q\\\"x\"")))
      (is (equal '("a" "" "b") (exec-list "a \"\" b"))))

(test split-exec-field-codes
      "Field codes are dropped and %% becomes %."
      (is (equal '("app" "100%") (exec-list "app %f %U 100%%"))))

(test split-exec-empty
      "A blank line gives no arguments."
      (is (equal '() (exec-list "   ")))
      (is (equal '() (exec-list ""))))

(test find-sessions-reads-name-and-command
      "A session takes its name and command from Name and Exec."
      (with-temp-dir (dir)
        (write-text (format nil "~Awayland-sessions/sway.desktop" dir)
                    (desktop-entry "Sway" "sway --unsupported-gpu"
                                   "Type=Application"
                                   "DesktopNames=sway;wlroots"))
        (let ((sessions (ses:find-sessions (list dir))))
          (is (equal '("Sway") (session-names sessions)))
          (is (equal '("sway" "--unsupported-gpu")
                     (session-cmd (first sessions)))))))

(test find-sessions-skips-unusable-entries
      "Hidden, NoDisplay and incomplete entries are skipped."
      (with-temp-dir (dir)
        (flet ((put (file text)
                 (write-text (format nil "~Awayland-sessions/~A" dir file)
                             text)))
          (put "ok.desktop" (desktop-entry "Ok" "ok"))
          (put "hidden.desktop" (desktop-entry "Hid" "h" "Hidden=true"))
          (put "nodisplay.desktop" (desktop-entry "ND" "n" "NoDisplay=true"))
          (put "noexec.desktop" (format nil "[Desktop Entry]~%Name=NE~%"))
          (put "noname.desktop" (format nil "[Desktop Entry]~%Exec=x~%"))
          (put "readme.txt" (desktop-entry "Text" "t")))
        (is (equal '("Ok") (session-names (ses:find-sessions (list dir)))))))

(test find-sessions-reads-only-the-entry-group
      "Only unlocalized keys of [Desktop Entry] are used; # lines are skipped."
      (with-temp-dir (dir)
        (write-text (format nil "~Awayland-sessions/a.desktop" dir)
                    (format nil "# comment~%[Desktop Entry]~%~
                                 Name = Sway~%Name[fr]=Sway FR~%Exec = sway~%~
                                 [Desktop Action x]~%Name=Other~%~
                                 Exec=other~%"))
        (let ((sessions (ses:find-sessions (list dir))))
          (is (equal '("Sway") (session-names sessions)))
          (is (equal '("sway") (session-cmd (first sessions)))))))

(test find-sessions-sorts-files-by-name
      "Sessions of one directory come in file name order."
      (with-temp-dir (dir)
        (write-text (format nil "~Awayland-sessions/z.desktop" dir)
                    (desktop-entry "Zed" "z"))
        (write-text (format nil "~Awayland-sessions/a.desktop" dir)
                    (desktop-entry "Able" "a"))
        (is (equal '("Able" "Zed")
                   (session-names (ses:find-sessions (list dir)))))))

(test find-sessions-earlier-directory-wins
      "A name found in an earlier directory hides the same name later."
      (with-temp-dir (dir)
        (let ((one (format nil "~Aone" dir))
              (two (format nil "~Atwo" dir)))
          (write-text (format nil "~A/wayland-sessions/sway.desktop" one)
                      (desktop-entry "Sway" "sway"))
          (write-text (format nil "~A/wayland-sessions/sway.desktop" two)
                      (desktop-entry "Sway" "other"))
          (write-text (format nil "~A/wayland-sessions/hypr.desktop" two)
                      (desktop-entry "Hypr" "Hyprland"))
          (let ((sessions (ses:find-sessions (list one two))))
            (is (equal '("Sway" "Hypr") (session-names sessions)))
            (is (equal '("sway") (session-cmd (first sessions))))))))

(test find-sessions-missing-directory
      "A directory that does not exist gives no sessions."
      (is (null (ses:find-sessions (list "/nonexistent/cloak")))))

(test xdg-data-dirs-from-environment
      "XDG_DATA_DIRS is split at colons; empty entries are dropped."
      (with-env ("XDG_DATA_DIRS" "/a::/b/")
        (is (equal '("/a" "/b/") (ses::xdg-data-dirs)))))

(test xdg-data-dirs-default
      "An unset or empty XDG_DATA_DIRS gives the XDG default."
      (dolist (value '(nil ""))
        (with-env ("XDG_DATA_DIRS" value)
          (is (equal '("/usr/local/share" "/usr/share")
                     (ses::xdg-data-dirs))))))

(test find-sessions-defaults-to-environment
      "FIND-SESSIONS searches XDG_DATA_DIRS when given no directories."
      (with-temp-dir (dir)
        (write-text (format nil "~Awayland-sessions/sway.desktop" dir)
                    (desktop-entry "Sway" "sway"))
        (with-env ("XDG_DATA_DIRS" dir)
          (is (equal '("Sway") (session-names (ses:find-sessions)))))))


;;; users

(defparameter *passwd*
  (format nil "~{~A~%~}"
          '("root:x:0:0:root:/root:/bin/bash"
            "nobody:x:65534:65534::/nonexistent:/usr/sbin/nologin"
            "locutus:x:1000:1000:L:/home/locutus:/bin/bash"
            "svc:x:1001:1001::/var/svc:/run/current-system/sbin/nologin"
            "off:x:1002:1002::/h:/bin/false"
            "plain:x:1003:1003::/h:"
            ":x:1004:1::/h:/bin/sh"
            "bad:x:abc:1::/h:/bin/sh"
            "short:x:1"
            "builder:x:30001:1::/h:/bin/sh"
            "#comment"))
  "A passwd file with login accounts and several that must be skipped.")

(test parse-passwd-line-valid
      "A well formed line gives the name, uid and shell."
      (is (equal '("a" 5 "/bin/sh")
                 (multiple-value-list
                  (usr::parse-passwd-line "a:x:5:6:g:/h:/bin/sh")))))

(test parse-passwd-line-malformed
      "Lines with too few fields, no name or a bad uid give nil."
      (dolist (line '("short:x:1" ":x:1:1::/h:/bin/sh" "a:x:abc:1::/h:/bin/sh"
                      "a:x::1::/h:/bin/sh" "" "#comment"))
        (is (null (usr::parse-passwd-line line)))))

(test login-user-p-uid-range
      "Only UIDs from 1000 up to 59999 are login accounts."
      (is-false (usr::login-user-p 999 "/bin/bash"))
      (is-true (usr::login-user-p 1000 "/bin/bash"))
      (is-true (usr::login-user-p 59999 "/bin/bash"))
      (is-false (usr::login-user-p 60000 "/bin/bash")))

(test login-user-p-shell
      "The nologin and false shells are refused; an empty shell is not."
      (is-false (usr::login-user-p 1000 "/usr/sbin/nologin"))
      (is-false (usr::login-user-p 1000 "/run/current-system/sbin/nologin"))
      (is-false (usr::login-user-p 1000 "/bin/false"))
      (is-false (usr::login-user-p 1000 "nologin"))
      (is-true (usr::login-user-p 1000 ""))
      (is-true (usr::login-user-p 1000 "/bin/bash")))

(test list-users-reads-passwd-file
      "Login accounts are listed in file order, with their uids."
      (with-temp-dir (dir)
        (let ((users (usr:list-users
                      (write-text (format nil "~Apasswd" dir) *passwd*))))
          (is (equal '("locutus" "plain" "builder")
                     (mapcar #'usr:user-name users)))
          (is (equal '(1000 1003 30001) (mapcar #'usr:user-uid users))))))

(test list-users-missing-file
      "A missing passwd file warns once and gives no users."
      (multiple-value-bind (users warnings)
          (collect-warnings
           (lambda () (usr:list-users "/nonexistent/cloak-passwd")))
        (is (null users))
        (is (= 1 (length warnings)))))


;;; state

(defparameter *empty-state* '(:last-user nil :sessions nil)
  "What LOAD-STATE returns when there is nothing saved.")

(test load-state-missing-file
      "A missing state file gives the empty state, with no warning."
      (multiple-value-bind (state warnings)
          (collect-warnings
           (lambda () (ste:load-state "/nonexistent/cloak-state.sexp")))
        (is (equal *empty-state* state))
        (is (null warnings))))

(test state-round-trip
      "SAVE-STATE writes what LOAD-STATE reads back."
      (with-temp-dir (dir)
        (let ((path (format nil "~Astate.sexp" dir))
              (state (ste:remember-login *empty-state* "locutus" "Sway")))
          (is-true (ste:save-state path state))
          (is (equal state (ste:load-state path))))))

(test save-state-unwritable
      "A state that cannot be written warns once and returns nil."
      (multiple-value-bind (saved warnings)
          (collect-warnings
           (lambda ()
             (ste:save-state "/nonexistent/dir/state.sexp" *empty-state*)))
        (is (null saved))
        (is (= 1 (length warnings)))))

(test load-state-bad-files
      "Wrong shape, bad syntax and #. each warn once and give the empty state."
      (with-temp-dir (dir)
        (dolist (text '("(:last-user 5)" "(:sessions ((\"a\" . 1)))"
                        "(:last-user" "#.(+ 1 2)"))
          (multiple-value-bind (state warnings)
              (collect-warnings
               (lambda ()
                 (ste:load-state
                  (write-text (format nil "~Astate.sexp" dir) text))))
            (is (equal *empty-state* state))
            (is (= 1 (length warnings)))))))

(test remember-login-sets-user-and-session
      "REMEMBER-LOGIN records the last user and that user's session."
      (let ((state (ste:remember-login *empty-state* "locutus" "Sway")))
        (is (string= "locutus" (ste:state-last-user state)))
        (is (string= "Sway" (ste:state-last-session state "locutus")))
        (is (null (ste:state-last-session state "someone")))))

(test remember-login-replaces-and-keeps-others
      "A new login replaces that user's session and keeps other users'."
      (let* ((first (ste:remember-login *empty-state* "locutus" "Sway"))
             (second (ste:remember-login first "other" "Hypr"))
             (third (ste:remember-login second "locutus" "Hypr")))
        (is (string= "locutus" (ste:state-last-user third)))
        (is (string= "Hypr" (ste:state-last-session third "locutus")))
        (is (string= "Hypr" (ste:state-last-session third "other")))
        (is (= 2 (length (getf third :sessions))))))

(test remember-login-does-not-change-its-argument
      "REMEMBER-LOGIN returns a new state and leaves the old one alone."
      (let* ((old (ste:remember-login *empty-state* "locutus" "Sway"))
             (copy (copy-tree old)))
        (ste:remember-login old "locutus" "Hypr")
        (is (equal copy old))))
