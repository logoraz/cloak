;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:cloak/ui/form
  (:use #:cl)
  (:import-from #:gtk
                #:+align-center+
                #:+orientation-horizontal+
                #:+orientation-vertical+
                #:box-append
                #:connect
                #:drop-down-selected
                #:editable-text
                #:label-text
                #:make-application-window
                #:make-box
                #:make-button
                #:make-drop-down
                #:make-label
                #:make-password-entry
                #:password-entry-show-peek-icon-p
                #:run-in-main-event-loop
                #:timeout-add-seconds
                #:widget-add-css-class
                #:widget-grab-focus
                #:widget-halign
                #:widget-remove-css-class
                #:widget-sensitive-p
                #:widget-valign
                #:window-child
                #:window-focus)
  (:import-from #:gio
                #:application-quit)
  (:import-from #:adw
                #:avatar-custom-image
                #:avatar-text
                #:make-avatar)
  (:import-from #:gdk
                #:make-texture)
  (:local-nicknames (#:log #:cloak/lib/logging)
                    (#:lgn #:cloak/base/login)
                    (#:cfg #:cloak/base/config)
                    (#:ses #:cloak/base/sessions)
                    (#:usr #:cloak/base/users)
                    (#:ste #:cloak/base/state)
                    (#:thm #:cloak/ui/theme))
  (:export #:make-greeter
           #:make-form-window)
  (:documentation "The login form, and the login flow behind it.

A GREETER holds what lasts for the whole run: the settings, the login with
greetd, and the form on screen. Each call of MAKE-FORM-WINDOW builds a new
form, since the window can move to another output when monitors change."))

(in-package #:cloak/ui/form)


;;; Code:

(defclass greeter ()
  ((app :initarg :app :reader greeter-app
        :documentation "The GTK application.")
   (config :initarg :config :reader greeter-config
           :documentation "The loaded configuration.")
   (users :initarg :users :reader greeter-users
          :documentation "The users that can log in.")
   (sessions :initarg :sessions :reader greeter-sessions
             :documentation "The desktop sessions that can start.")
   (state :initarg :state :accessor greeter-state
          :documentation "What was remembered from earlier logins.")
   (state-path :initarg :state-path :reader greeter-state-path
               :documentation "Where the state is saved.")
   (login :initform nil :accessor greeter-login
          :documentation "The login with greetd, or nil.")
   (form :initform nil :accessor greeter-form
         :documentation "The form on screen, or nil.")
   (secret :initform nil :accessor greeter-secret
           :documentation "The password typed before the first prompt.")
   (awaiting :initform nil :accessor greeter-awaiting
             :documentation "True while a later prompt waits for an answer.")
   (choice :initform nil :accessor greeter-choice
           :documentation "The user name and DESKTOP-SESSION of the login in
progress, as a cons."))
  (:documentation "What the greeter keeps for the whole run."))

(defclass form ()
  ((greeter :initarg :greeter :reader form-greeter
            :documentation "The GREETER this form belongs to.")
   (box :initform nil :accessor form-box
        :documentation "The top widget, put in the window.")
   (avatar :initform nil :accessor form-avatar
           :documentation "The avatar of the selected user.")
   (user-picker :initform nil :accessor form-user-picker)
   (session-picker :initform nil :accessor form-session-picker)
   (entry :initform nil :accessor form-entry
          :documentation "The password entry.")
   (status :initform nil :accessor form-status
           :documentation "The label for prompts and errors.")
   (clock :initform nil :accessor form-clock)
   (alive :initform t :accessor form-alive
          :documentation "True until the window is destroyed."))
  (:documentation "The widgets of one login form."))


;;; Clock

(defparameter *day-names* '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "Short day names, Monday first, as DECODE-UNIVERSAL-TIME counts them.")

(defparameter *month-names*
  '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "Short month names.")

(defun format-clock (control &optional (time (get-universal-time)))
  "Return TIME as text made by CONTROL, a string with these strftime style
directives: %H %I %M %S %p %Y %m %d %a %b and %%. Any other directive is
copied unchanged."
  (multiple-value-bind (second minute hour day month year weekday)
      (decode-universal-time time)
    (with-output-to-string (out)
      (loop :with index := 0
            :while (< index (length control))
            :do (let ((char (char control index)))
                  (cond
                    ((and (char= char #\%) (< (1+ index) (length control)))
                     (let ((code (char control (1+ index))))
                       (case code
                         (#\H (format out "~2,'0D" hour))
                         (#\I (format out "~2,'0D"
                                      (if (zerop (mod hour 12))
                                          12
                                          (mod hour 12))))
                         (#\M (format out "~2,'0D" minute))
                         (#\S (format out "~2,'0D" second))
                         (#\p (write-string (if (< hour 12) "AM" "PM") out))
                         (#\Y (format out "~D" year))
                         (#\m (format out "~2,'0D" month))
                         (#\d (format out "~2,'0D" day))
                         (#\a (write-string (nth weekday *day-names*) out))
                         (#\b (write-string (nth (1- month) *month-names*)
                                            out))
                         (#\% (write-char #\% out))
                         (t (write-char char out)
                            (write-char code out)))
                       (incf index 2)))
                    (t (write-char char out)
                       (incf index))))))))

(defun update-clock (form)
  "Show the current time in the clock label of FORM."
  (setf (label-text (form-clock form))
        (format-clock (cfg:config-ref (greeter-config (form-greeter form))
                                      :clock :format))))


;;; Choices

(defun user-names (greeter)
  "Return the login names of the users of GREETER."
  (mapcar #'usr:user-name (greeter-users greeter)))

(defun session-names (greeter)
  "Return the names of the sessions of GREETER."
  (mapcar #'ses:desktop-session-name (greeter-sessions greeter)))

(defun name-index (name names)
  "Return the position of NAME in NAMES, or 0 if it is not there."
  (or (position name names :test #'equal) 0))

(defun initial-user (greeter)
  "Return the name of the user to preselect: the last one to log in, else
the user named in the config, else nil, which selects the first."
  (or (ste:state-last-user (greeter-state greeter))
      (cfg:config-ref (greeter-config greeter) :user :name)))

(defun initial-session (greeter user)
  "Return the name of the session to preselect for USER: the one USER used
last, else the session named in the config, else nil."
  (or (ste:state-last-session (greeter-state greeter) user)
      (cfg:config-ref (greeter-config greeter) :session :default)))

(defun selected-user (form)
  "Return the login name picked in FORM, or nil if there is none."
  (nth (drop-down-selected (form-user-picker form))
       (user-names (form-greeter form))))

(defun selected-session (form)
  "Return the DESKTOP-SESSION picked in FORM, or nil if there is none."
  (nth (drop-down-selected (form-session-picker form))
       (greeter-sessions (form-greeter form))))

(defun select-remembered-session (form)
  "Pick in FORM the session that the selected user used last, if any."
  (let* ((greeter (form-greeter form))
         (name (ste:state-last-session (greeter-state greeter)
                                       (selected-user form))))
    (when name
      (setf (drop-down-selected (form-session-picker form))
            (name-index name (session-names greeter))))))


;;; Widgets

(defparameter *avatar-size* 96
  "The size of the avatar, in pixels.")

(defun avatar-texture (path)
  "Return the image at PATH as a texture. If PATH is nil or the image cannot
be read, return nil, with a warning for the second case."
  (when path
    (handler-case (make-texture :path path)
      (error (condition)
        (warn "avatar: cannot read ~A: ~A" path condition)
        nil))))

(defun update-avatar (form)
  "Show the avatar of the user picked in FORM. The image from the config's
:user :avatar belongs to the user named by :user :name; every other user
gets the initials of their login name."
  (let* ((config (greeter-config (form-greeter form)))
         (user (selected-user form))
         (avatar (form-avatar form))
         (mine (and user
                    (equal user (cfg:config-ref config :user :name)))))
    (setf (avatar-text avatar) (or user "")
          (avatar-custom-image avatar)
          (and mine (avatar-texture (cfg:config-ref config :user :avatar))))))

(defun show-status (form text &optional error-p)
  "Show TEXT under the password entry of FORM, styled as an error if ERROR-P
is true."
  (let ((label (form-status form)))
    (setf (label-text label) text)
    (if error-p
        (widget-add-css-class label "cloak-error")
        (widget-remove-css-class label "cloak-error"))))

(defun set-busy (form busy)
  "Lock the pickers and the password entry of FORM while BUSY is true, and
unlock them, with the focus on the entry, when it is false."
  (dolist (widget (list (form-user-picker form)
                        (form-session-picker form)
                        (form-entry form)))
    (setf (widget-sensitive-p widget) (not busy)))
  (unless busy
    (widget-grab-focus (form-entry form))))

(defun make-picker (names selected)
  "Return a drop-down of NAMES with the one at index SELECTED chosen."
  (let ((picker (make-drop-down :strings names)))
    (setf (drop-down-selected picker) selected)
    picker))

(defun run-command (cmd)
  "Run the command CMD, a list of strings, without waiting for it."
  (handler-case (uiop:launch-program cmd)
    (error (condition)
      (log:log-message "form: command ~S failed: ~A" cmd condition))))

(defun make-power-button (label cmd)
  "Return a button named LABEL that runs the command CMD."
  (let ((button (make-button :label label)))
    (connect button "clicked"
                 (lambda (button)
                   (declare (ignore button))
                   (run-command cmd)))
    button))

(defun build-form (greeter)
  "Build the widgets of a login form for GREETER and return the FORM."
  (let* ((config (greeter-config greeter))
         (form (make-instance 'form :greeter greeter))
         (box (make-box :orientation +orientation-vertical+
                            :spacing 10))
         (power (make-box :orientation +orientation-horizontal+
                              :spacing 10))
         (user (initial-user greeter))
         (user-picker (make-picker (user-names greeter)
                                   (name-index user (user-names greeter))))
         (session-picker (make-picker
                          (session-names greeter)
                          (name-index (initial-session greeter user)
                                      (session-names greeter))))
         (entry (make-password-entry)))
    (setf (form-box form) box
          (form-avatar form) (make-avatar :size *avatar-size*
                                          :text ""
                                          :show-initials t)
          (form-user-picker form) user-picker
          (form-session-picker form) session-picker
          (form-entry form) entry
          (form-status form) (make-label :str "")
          (form-clock form) (make-label :str ""))
    (widget-add-css-class box "cloak-form")
    (setf (password-entry-show-peek-icon-p entry) t)
    (setf (widget-halign power) +align-center+)
    (widget-add-css-class (form-clock form) "cloak-clock")
    (setf (widget-halign box) +align-center+
          (widget-valign box) +align-center+)
    (connect user-picker "notify::selected"
                 (lambda (picker pspec)
                   (declare (ignore picker pspec))
                   (select-remembered-session form)
                   (update-avatar form)))
    (connect entry "activate"
                 (lambda (entry)
                   (declare (ignore entry))
                   (submit form)))
    (box-append power
                    (make-power-button
                     "Reboot" (cfg:config-ref config :commands :reboot)))
    (box-append power
                    (make-power-button
                     "Power off" (cfg:config-ref config :commands :poweroff)))
    (dolist (widget (list (make-label
                           :str (cfg:config-ref config :greeting))
                          (form-clock form)
                          (form-avatar form)
                          user-picker
                          session-picker
                          entry
                          (form-status form)
                          power))
      (box-append box widget))
    (update-avatar form)
    (update-clock form)
    (timeout-add-seconds 1 (lambda ()
                                 (when (form-alive form)
                                   (update-clock form))
                                 (form-alive form)))
    form))


;;; Login

(defun reset-login (greeter text)
  "Forget the login attempt of GREETER, unlock its form and show TEXT there
as an error."
  (setf (greeter-secret greeter) nil
        (greeter-awaiting greeter) nil)
  (let ((form (greeter-form greeter)))
    (when form
      (setf (editable-text (form-entry form)) "")
      (show-status form text t)
      (set-busy form nil))))

(defgeneric submit (form)
  (:documentation "Handle Enter in the password entry of FORM."))

(defmethod submit ((form form))
  "Handle Enter in the password entry of FORM: answer the prompt that waits,
or start a login for the selected user."
  (let* ((greeter (form-greeter form))
         (login (greeter-login greeter))
         (text (editable-text (form-entry form)))
         (user (selected-user form))
         (session (selected-session form)))
    (cond
      ((null login) (show-status form "Cannot reach greetd." t))
      ((null user) (show-status form "There is no user to log in." t))
      ((null session) (show-status form "There is no session to start." t))
      (t
       (set-busy form t)
       (show-status form "")
       (setf (editable-text (form-entry form)) "")
       (handler-case
           (cond ((greeter-awaiting greeter)
                  (setf (greeter-awaiting greeter) nil)
                  (lgn:answer-prompt login text))
                 (t
                  (setf (greeter-secret greeter) text
                        (greeter-choice greeter) (cons user session))
                  (lgn:begin-login login user)))
         (error (condition)
           (log:log-message "form: ~A" condition)
           (reset-login greeter "Cannot start the login.")))))))

(defun finish (greeter)
  "Remember the user and session of the login that succeeded, then quit, so
that greetd starts the session."
  (destructuring-bind (user . session) (greeter-choice greeter)
    (setf (greeter-state greeter)
          (ste:remember-login (greeter-state greeter) user
                              (ses:desktop-session-name session)))
    (ste:save-state (greeter-state-path greeter) (greeter-state greeter))
    (log:log-message "starting ~A for ~A" (ses:desktop-session-name session)
                     user))
  (application-quit (greeter-app greeter)))

(defgeneric handle-event (greeter kind &rest args)
  (:documentation "React to an event of the login flow. KIND is the first
element of the event, such as :PROMPT, and ARGS the rest. Run this in the
main event loop."))

(defmethod handle-event ((greeter greeter) (kind (eql :prompt)) &rest args)
  "Answer the first prompt with the typed password; show later ones."
  (let ((form (greeter-form greeter)))
    (cond ((greeter-secret greeter)
           (lgn:answer-prompt (greeter-login greeter)
                              (greeter-secret greeter))
           (setf (greeter-secret greeter) nil))
          (form
           (setf (greeter-awaiting greeter) t)
           (show-status form (second args))
           (set-busy form nil)))))

(defmethod handle-event ((greeter greeter) (kind (eql :message)) &rest args)
  "Show an informational message from greetd."
  (let ((form (greeter-form greeter)))
    (when form
      (show-status form (second args)))))

(defmethod handle-event ((greeter greeter) (kind (eql :ready)) &rest args)
  "Start the chosen session once greetd accepts the login."
  (declare (ignore args))
  (lgn:launch (greeter-login greeter)
              (ses:desktop-session-cmd (cdr (greeter-choice greeter)))
              #()))

(defmethod handle-event ((greeter greeter) (kind (eql :started)) &rest args)
  "Finish when greetd reports the session started."
  (declare (ignore args))
  (finish greeter))

(defmethod handle-event ((greeter greeter) (kind (eql :auth-failed))
                         &rest args)
  "Report a failed login and unlock the form."
  (log:log-message "login failed: ~A" (first args))
  (reset-login greeter "Login failed. Try again."))

(defmethod handle-event ((greeter greeter) (kind (eql :error)) &rest args)
  "Report an error from greetd and unlock the form."
  (log:log-message "login error: ~A" (first args))
  (reset-login greeter (first args)))

(defun dispatch-event (greeter event)
  "Run HANDLE-EVENT for EVENT, a list from the login flow such as (:prompt
:secret \"Password:\"). Errors are logged and unlock the form."
  (handler-case (apply #'handle-event greeter event)
    (error (condition)
      (log:log-message "form: ~A" condition)
      (reset-login greeter "Something went wrong."))))

(defun connect-login (greeter)
  "Return a login with greetd that reports its events to GREETER. If greetd
cannot be reached, warn and return nil."
  (handler-case
      (lgn:make-login
       (lambda (event)
         (run-in-main-event-loop ()
           (dispatch-event greeter event))))
    (error (condition)
      (warn "login: cannot reach greetd: ~A" condition)
      nil)))

(defun make-greeter (&key app config users sessions state state-path)
  "Return a GREETER for the GTK application APP, with the loaded CONFIG,
USERS, SESSIONS and STATE. STATE-PATH is where the state is saved."
  (let ((greeter (make-instance 'greeter :app app :config config
                                         :users users :sessions sessions
                                         :state state
                                         :state-path state-path)))
    (setf (greeter-login greeter) (connect-login greeter))
    greeter))


;;; Window

(defun make-form-window (greeter)
  "Return a new window of the application of GREETER that shows the login
form. The form becomes the one on screen until its window is destroyed."
  (let* ((window (thm:style-window
                  (make-application-window
                   :application (greeter-app greeter))))
         (form (build-form greeter)))
    (setf (greeter-form greeter) form
          (window-child window) (form-box form)
          (window-focus window) (form-entry form))
    (connect window "destroy"
                 (lambda (window)
                   (declare (ignore window))
                   (setf (form-alive form) nil)
                   (when (eq form (greeter-form greeter))
                     (setf (greeter-form greeter) nil))))
    window))
