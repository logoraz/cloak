(defpackage #:cloak/base/greetd-ipc
  (:use #:cl)
  (:import-from #:cloak/lib/jzon-message
                #:define-message
                #:decode-message)
  (:local-nicknames (#:ext #:sb-ext)
                    (#:sox #:sb-bsd-sockets))
  ;; Requests
  (:export #:make-create-session
           #:create-session->json
           #:make-post-auth-response
           #:post-auth-response->json
           #:make-start-session
           #:start-session->json
           #:make-cancel-session
           #:cancel-session->json)
  ;; Responses
  (:export #:success-p
           #:error-reply-p
           #:error-reply-error-type
           #:error-reply-description
           #:auth-message-p
           #:auth-message-auth-message-type
           #:auth-message-auth-message)
  ;; Framing
  (:export #:length->octets
           #:octets->length
           #:read-octets
           #:write-frame
           #:read-frame)
  ;; Transport
  (:export #:greetd-socket-path
           #:connect-greetd
           #:request->json
           #:send-request
           #:receive-response
           #:exchange)
  (:documentation "greetd IPC protocol over a Unix domain socket."))

(in-package #:cloak/base/greetd-ipc)


;;; Requests

(define-message create-session "create_session" (username)
  "Creates a session and initiates a login attempt for the given user.
The session is ready to be started if a success is returned.

\"<MESSAGE TYPE>\" (FIELDS)

FIELDS:
- username (string)
")

(define-message post-auth-response "post_auth_message_response" (response)
"Answers an authentication message. If the message was informative
(info, error), then a response does not need to be set in this message.
The session is ready to be started if a success is returned.

\"<MESSAGE TYPE>\" (FIELDS)

FIELDS:
- response (string, optional)
")

(define-message start-session "start_session" (cmd env)
"Requests for the session to be started using the provided command line,
adding the supplied environment to that created by PAM. The session will
start after the greeter process and terminates.

\"<MESSAGE TYPE>\" (FIELDS)

FIELDS:
- cmd (array of strings),
- env (array of strings)
")

(define-message cancel-session "cancel_session" ()
"Cancels the session that is currently under configuration.

\"<MESSAGE TYPE>\" (FIELDS)

FIELDS: none
")


;;; Responses

(define-message success "success" ()
"Indicates that a request succeeded.

\"<MESSAGE TYPE>\" (FIELDS)

FIELDS: none
")

(define-message error-reply "error"
  ((error-type :enum (auth-error error)) description)
  "Indicates that the request failed.

\"<MESSAGE TYPE>\" (FIELDS)

FIELDS:
- error_type (enum as string),
- description (string)

ENUMS:
- auth_error . Indicates that authentication failed. This is not a fatal error,
               and is likely caused by incorrect credentials. Handle as
               appropriate.

- error      . A general error. See the error description for more information.
")

(define-message auth-message "auth_message"
  ((auth-message-type :enum (visible secret info error)) auth-message)
 "Indicates that an authentication message needs to be answered to continue
through the authentication flow. There are no limits on the number and type
of messages that may be required for authentication to succeed, and a
greeter should not make any assumptions about the messages. Must be
answered with either post_auth_message_response or cancel_session.

\"<MESSAGE TYPE>\" (FIELDS)

FIELDS:
- auth_message_type (enum as string),
- auth_message (string)

ENUMS:
 - visible . Indicates that the input from the user should be visible when
             they answer this question.

 - secret  . Indicates that the input from the user should be considered
             secret when they answer this question.

 - info    . Indicates that this message is informative, not a question.

 - error   . Indicates that this message is an error, not a question.
")


;;; Framing

(defun length->octets (n)
  "Return N, an integer from 0 to 2^32 - 1, as a vector of 4 octets.
The octets are in the native byte order of this machine, as greetd
expects. Signal an error if N is out of range."
  (check-type n (unsigned-byte 32))
  (let ((vec (make-array 4 :element-type '(unsigned-byte 8))))
    (dotimes (i 4)
      (setf (aref vec i) (ldb (byte 8 (* 8 i)) n)))
    vec))

(defun octets->length (octets)
  "Return the integer held in OCTETS, a vector of exactly 4 octets.
The octets are in the native byte order. This reverses LENGTH->OCTETS.
Signal an error if OCTETS does not have 4 elements."
  (check-type octets (vector (unsigned-byte 8) 4))
  (let ((n 0))
    (dotimes (i 4)
      (incf n (ash (aref octets i) (* 8 i))))
    n))

(defun read-octets (stream count)
  "Read exactly COUNT octets from the binary STREAM and return them.
Signal END-OF-FILE if the stream ends before COUNT octets arrive."
  (let* ((vec (make-array count :element-type '(unsigned-byte 8)))
         (end (read-sequence vec stream)))
    (when (< end count)
      (error 'end-of-file :stream stream))
    vec))

(defun write-frame (stream string)
  "Write STRING to the binary STREAM as one greetd frame.
The frame is a 4-octet header holding the number of octets in STRING,
followed by those octets, encoded as UTF-8. Flush the stream before
returning."
  (let* ((payload (ext:string-to-octets string :external-format :utf-8))
         (count (length payload))
         (vec (make-array (+ 4 count) :element-type '(unsigned-byte 8)))
         (header (length->octets count)))
    ;; Convert vec into frame -> header + payload
    ;; header is the size of the payload represented in 4 octets
    (replace vec header)            ; header into elements 0 to 3
    (replace vec payload :start1 4) ; payload into elements 4 onward
    (write-sequence vec stream)
    (finish-output stream)))

(defun read-frame (stream)
  "Read one greetd frame from the binary STREAM and return its string.
Read 4 octets for the length, then that many octets, and decode them
as UTF-8. Signal END-OF-FILE if the stream ends inside a frame."
  (let* ((header (read-octets stream 4))
         (count (octets->length header))
         (payload (read-octets stream count)))
    (ext:octets-to-string payload :external-format :utf-8)))


;;; Transport

(defun greetd-socket-path ()
  "Return the greetd socket path from the GREETD_SOCK environment variable.
Signal an error if the variable is not set."
  (or (ext:posix-getenv "GREETD_SOCK")
      (error "GREETD_SOCK not set")))

(defun connect-greetd (path)
  "Connect to the Unix socket at PATH and return a binary stream for it.
The stream reads and writes octets."
  (let ((socket (make-instance 'sox:local-socket :type :stream)))
    (sox:socket-connect socket path)
    (sox:socket-make-stream socket :input t :output t
                                       :element-type '(unsigned-byte 8)
                                       :buffering :full)))

(defun request->json (request)
  "Return REQUEST as a JSON string, by calling the writer for its type.
REQUEST is a CREATE-SESSION, POST-AUTH-RESPONSE, START-SESSION or
CANCEL-SESSION. Signal an error for any other object."
  (etypecase request
    (create-session (create-session->json request))
    (post-auth-response (post-auth-response->json request))
    (start-session (start-session->json request))
    (cancel-session (cancel-session->json request))))

(defun send-request (stream request)
  "Write REQUEST to the binary STREAM as one frame."
  (let ((json (request->json request)))
    (write-frame stream json)))

(defun receive-response (stream)
  "Read one frame from the binary STREAM and return the response it holds.
The response is a SUCCESS, ERROR-REPLY or AUTH-MESSAGE."
  (decode-message (read-frame stream)))

(defun exchange (stream request)
  "Send REQUEST on STREAM, then return the response greetd sends back.
greetd answers every request with exactly one response."
  (send-request stream request)
  (receive-response stream))
