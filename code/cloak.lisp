(defpackage #:cloak
  (:use #:cl)
  (:import-from #:gtk
                #:application-run
                #:connect
                #:make-application)
  (:import-from #:gio
                #:+application-flags-flags-none+
                #:application-hold)
  (:local-nicknames (#:ext #:sb-ext)
                    (#:log #:cloak/lib/logging)
                    (#:ipc #:cloak/base/greetd-ipc)
                    (#:lgn #:cloak/base/login)
                    (#:cfg #:cloak/base/config)
                    (#:ses #:cloak/base/sessions)
                    (#:usr #:cloak/base/users)
                    (#:ste #:cloak/base/state)
                    (#:bke #:cloak/ui/backend)
                    (#:out #:cloak/ui/outputs)
                    (#:thm #:cloak/ui/theme)
                    (#:frm #:cloak/ui/form))
  (:export #:main)
  (:documentation "Main entry point for cloak."))

(in-package #:cloak)


;;; Entry Point

(defparameter *default-config* "/etc/greetd/cloak.sexp"
  "Where the config file is read from unless --config says otherwise.")

(defparameter *default-log* "/var/log/cloak/log"
  "Where the log is written unless --log says otherwise.")

(defparameter *state-path* "/var/lib/cloak/state.sexp"
  "Where the state is kept between logins.")

(defparameter *application-id* "org.cloak.greeter"
  "The GTK application id of the greeter.")

(defparameter *flags*
  '(("--config" . :config) ("--style" . :style) ("--log" . :log))
  "The command line flags that take a value, with their option keys.")

(defun parse-args (args)
  "Read the command line ARGS. Return two values: a plist with :config,
:style and :log, and the list of arguments that were not understood. A
flag with no value after it is not understood."
  (let ((options (list :config *default-config*
                       :style nil
                       :log *default-log*))
        (unknown '()))
    (loop :while args
          :do (let* ((arg (pop args))
                     (key (cdr (assoc arg *flags* :test #'string=))))
                (if (and key args)
                    (setf (getf options key) (pop args))
                    (push arg unknown))))
    (values options (nreverse unknown))))

(defun log-warnings (thunk)
  "Call THUNK, sending any warning it signals to the log instead of the
error output."
  (handler-bind ((warning (lambda (condition)
                            (log:log-message "warning: ~A" condition)
                            (muffle-warning condition))))
    (funcall thunk)))

(defun show-greeter (app config style greeter)
  "Apply the theme of CONFIG, with the CSS file at STYLE, which may be nil,
then open a window on each monitor: the form on the primary one and the
wallpaper on the others. Keep them in step as monitors come and go. Call
this when APP activates."
  (thm:apply-theme config style)
  (application-hold app)
  (let ((backend (bke:choose-backend)))
    (log:log-message "window backend: ~A" (bke:backend-name backend))
    (out:watch-outputs
     (out:make-outputs
      :backend backend
      :primary (cfg:config-ref config :layout :primary)
      :maker (lambda (role)
               (ecase role
                 (:form (frm:make-form-window greeter))
                 (:cover (thm:make-cover app))))))))

(defun run (options)
  "Load the config, state, users and sessions named by OPTIONS, then show
the greeter until a login succeeds."
  (let* ((config (cfg:load-config (getf options :config)))
         (state (ste:load-state *state-path*))
         (users (usr:list-users))
         (sessions (ses:find-sessions))
         (app (make-application :application-id *application-id*
                                :flags +application-flags-flags-none+))
         (greeter (frm:make-greeter :app app
                                    :config config
                                    :users users
                                    :sessions sessions
                                    :state state
                                    :state-path *state-path*)))
    (log:log-message "~D users, ~D sessions"
                     (length users) (length sessions))
    (connect app "activate"
             (lambda (app)
               (show-greeter app config (getf options :style) greeter)))
    (application-run app nil)))

(defun main ()
  "Run the greeter. Read the flags, open the log, then run; close the log
on the way out."
  (multiple-value-bind (options unknown)
      (parse-args (rest ext:*posix-argv*))
    (log:open-log (getf options :log))
    (unwind-protect
         (log-warnings
          (lambda ()
            (dolist (arg unknown)
              (warn "unknown argument ~A" arg))
            (run options)))
      (log:close-log))))
