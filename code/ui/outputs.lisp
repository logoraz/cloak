(defpackage #:cloak/ui/outputs
  (:use #:cl)
  (:import-from #:gtk
                #:connect
                #:window-destroy
                #:window-present)
  (:import-from #:gdk
                #:display-default
                #:display-monitors
                #:monitor-connector)
  (:import-from #:gio
                #:list-model-get-item
                #:list-model-n-items)
  (:local-nicknames (#:bke #:cloak/ui/backend))
  (:export #:make-outputs
           #:watch-outputs)
  (:documentation "Multi-monitor greeter support."))

(in-package #:cloak/ui/outputs)


;;; Code:

(defclass output ()
  ((window :initarg :window :reader output-window
           :documentation "The window on the monitor.")
   (role :initarg :role :reader output-role
         :documentation "What the window shows: :FORM or :COVER."))
  (:documentation "The window on one monitor, and the role it plays."))

(defclass outputs ()
  ((backend :initarg :backend :reader outputs-backend
            :documentation "The BACKEND that puts a window on a monitor.")
   (maker :initarg :maker :reader outputs-maker
          :documentation "A function called with a role, :FORM or :COVER,
that returns a new window.")
   (primary :initarg :primary :initform nil :accessor outputs-primary
            :documentation "The connector name that should get the form, or
nil.")
   (windows :initform (make-hash-table :test #'equal)
            :reader outputs-windows
            :documentation "Maps connector names to OUTPUT objects."))
  (:documentation "The greeter windows, one for each monitor."))

(defun make-outputs (&key maker primary (backend (bke:choose-backend)))
  "Return an OUTPUTS that makes its windows with MAKER and puts the form on
the monitor named PRIMARY, which may be nil. BACKEND says how a window
covers a monitor; by default, the best one the compositor supports."
  (make-instance 'outputs :maker maker :primary primary :backend backend))

(defun monitor-model ()
  "Return the list model of the monitors of the default display."
  (display-monitors (display-default)))

(defun monitor-name (monitor position)
  "Return the connector name of MONITOR, such as eDP-1; if it has none,
return a name made from its POSITION in the list."
  (or (monitor-connector monitor)
      (format nil "monitor-~D" position)))

(defun connected-monitors ()
  "Return a list of (NAME . MONITOR) for each monitor plugged in."
  (let ((model (monitor-model)))
    (loop :for position :below (list-model-n-items model)
          :for monitor := (gobj:coerce
                           (list-model-get-item model position)
                           'gdk:monitor)
          :collect (cons (monitor-name monitor position) monitor))))

(defun primary-name (wanted names)
  "Return the name that gets the login form: WANTED if it is in NAMES,
otherwise the first of NAMES. Return nil if NAMES is empty."
  (if (member wanted names :test #'equal)
      wanted
      (first names)))

(defgeneric open-output (outputs name monitor role)
  (:documentation "Open a window for ROLE on MONITOR and keep it under
NAME."))

(defmethod open-output ((outputs outputs) name monitor role)
  "Only the form takes the keyboard."
  (let ((window (funcall (outputs-maker outputs) role)))
    (bke:prepare-window (outputs-backend outputs) window
                        :monitor monitor
                        :keyboard (if (eq role :form)
                                      :exclusive
                                      :none))
    (window-present window)
    (setf (gethash name (outputs-windows outputs))
          (make-instance 'output :window window :role role))))

(defgeneric close-output (outputs name)
  (:documentation "Close the window kept under NAME and forget it."))

(defmethod close-output ((outputs outputs) name)
  "The window may already be gone with its monitor, so errors are
ignored."
  (let ((output (gethash name (outputs-windows outputs))))
    (when output
      (ignore-errors (window-destroy (output-window output)))
      (remhash name (outputs-windows outputs)))))

(defgeneric window-names (outputs)
  (:documentation "Return the names that have a window now."))

(defmethod window-names ((outputs outputs))
  (loop :for name :being :the :hash-keys :of (outputs-windows outputs)
        :collect name))

(defgeneric sync-outputs (outputs)
  (:documentation "Make the windows match the monitors plugged in now."))

(defmethod sync-outputs ((outputs outputs))
  "Close the windows of monitors that are gone or whose role changed, then
open a window for each monitor that has none."
  (let* ((monitors (connected-monitors))
         (names (mapcar #'car monitors))
         (primary (primary-name (outputs-primary outputs) names))
         (windows (outputs-windows outputs)))
    (flet ((role-of (name)
             (if (equal name primary) :form :cover)))
      (dolist (name (window-names outputs))
        (unless (and (member name names :test #'equal)
                     (eq (role-of name)
                         (output-role (gethash name windows))))
          (close-output outputs name)))
      (loop :for (name . monitor) :in monitors
            :unless (gethash name windows)
            :do (open-output outputs name monitor (role-of name))))))

(defgeneric watch-outputs (outputs)
  (:documentation "Open the windows for the monitors plugged in now, and
keep them in step as monitors come and go."))

(defmethod watch-outputs ((outputs outputs))
  (connect (monitor-model) "items-changed"
           (lambda (model position removed added)
             (declare (ignore model position removed added))
             (sync-outputs outputs)))
  (sync-outputs outputs))
