(defpackage #:cloak/base/login
  (:use #:cl)
  (:local-nicknames (#:sbt #:sb-thread)
                    (#:ipc #:cloak/base/greetd-ipc))
  (:export #:make-login
           #:begin-login
           #:answer-prompt
           #:cancel-login
           #:launch)
  (:documentation "Login flow for the greeter, run on a worker thread"))

(in-package #:cloak/base/login)


;;; Code:

(defstruct (login (:copier nil) (:constructor %make-login))
  "One login attempt with greetd. Holds the connection, a lock that
serializes requests, the current state, and the UI callback."
  (stream nil)
  (lock (sbt:make-mutex :name "login"))
  (state)
  (callback))

(defun make-login (callback)
  "Connect to greetd and return a new LOGIN that reports to CALLBACK."
  (%make-login :stream (ipc:connect-greetd (ipc:greetd-socket-path))
               :callback callback
               :state :idle))

(defun emit (login event)
  "Call the callback of LOGIN with EVENT, a list such as (:ready)."
  (funcall (login-callback login) event))

(defun abort-login (login)
  "Set the state of LOGIN to :idle and tell greetd to cancel its session.
The reply is ignored."
  (setf (login-state login) :idle)
  (ipc:exchange (login-stream login) (ipc:make-cancel-session)))

(defun handle-response (login response)
  "Turn RESPONSE into an event, update the state of LOGIN, and call its
callback with the event.
The events are (:prompt TYPE TEXT), (:message TYPE TEXT), (:ready),
(:started), (:auth-failed TEXT) and (:error TEXT). An info or error
message from greetd is answered here, with no response text, so the
flow continues."
  (cond
    ((ipc:success-p response)
     (case (login-state login)
       (:in-progress (emit login '(:ready)))
       (:done (emit login '(:started)))))
    ((ipc:error-reply-p response)
     (let ((kind (if (eq :auth-error (ipc:error-reply-error-type response))
                     :auth-failed
                     :error)))
       (abort-login login)
       (emit login (list kind (ipc:error-reply-description response)))))
    ((ipc:auth-message-p response)
     (let ((type (ipc:auth-message-auth-message-type response))
           (text (ipc:auth-message-auth-message response)))
       (case type
         ((:visible :secret)
          (emit login (list :prompt type text)))
         (t
          (emit login (list :message type text))
          (handle-response
           login
           (ipc:exchange (login-stream login)
                         (ipc:make-post-auth-response)))))))))

(defun run-request (login request)
  "Send REQUEST to greetd on a new worker thread and return the thread.
The thread holds the lock of LOGIN while it waits for the reply, so only
one request is in flight. It passes the reply to HANDLE-RESPONSE. An
error on the thread resets LOGIN to :idle and emits (:error TEXT)."
  (sbt:make-thread
   (lambda ()
     (sbt:with-mutex ((login-lock login))
       (handler-case
           (handle-response
            login (ipc:exchange (login-stream login) request))
         (error (condition)
           (setf (login-state login) :idle)
           (emit login (list :error (princ-to-string condition)))))))
   :name "login-request"))

(defun begin-login (login username)
  "Start a login attempt for USERNAME by sending create_session.
Signal an error unless the state of LOGIN is :idle."
  (unless (eq :idle (login-state login))
    (error "A login attempt is already in progress."))
  (setf (login-state login) :in-progress)
  (run-request login (ipc:make-create-session :username username)))

(defun answer-prompt (login response)
  "Answer the current prompt with the string RESPONSE.
Signal an error unless the state of LOGIN is :in-progress."
  (unless (eq :in-progress (login-state login))
    (error "There is no login attempt to answer."))
  (run-request login (ipc:make-post-auth-response :response response)))

(defun cancel-login (login)
  "Cancel the attempt in progress with greetd and set the state of LOGIN
back to :idle, so a new attempt can begin."
  (setf (login-state login) :idle)
  (run-request login (ipc:make-cancel-session)))

(defun launch (login cmd env)
  "Ask greetd to start the desktop session with the command CMD and the
extra environment ENV, both vectors of strings. Set the state of LOGIN to
:done. The session starts after the greeter exits.
Signal an error unless the state of LOGIN is :in-progress."
  (unless (eq :in-progress (login-state login))
    (error "There is no login attempt to launch."))
  (setf (login-state login) :done)
  (run-request login (ipc:make-start-session :cmd cmd :env env)))
