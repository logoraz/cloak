;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:cloak/ui/backend
  (:use #:cl)
  (:import-from #:gtk
                #:window-fullscreen
                #:window-fullscreen-on-monitor)
  (:local-nicknames (#:lsh #:cloak/ui/layer-shell))
  (:export #:backend
           #:layer-shell-backend
           #:fullscreen-backend
           #:choose-backend
           #:backend-name
           #:prepare-window)
  (:documentation "How a greeter window is put on an output.

Each backend is a class. PREPARE-WINDOW turns a plain GTK window into a
window that covers one output. To support another way of doing this, make a
subclass of BACKEND, add a PREPARE-WINDOW method for it, and return it from
CHOOSE-BACKEND."))

(in-package #:cloak/ui/backend)


;;; Code:

(defclass backend ()
  ()
  (:documentation "The base class of the ways to cover an output."))

(defclass layer-shell-backend (backend)
  ()
  (:documentation "Cover outputs with layer-shell surfaces. The compositor
must support the layer-shell protocol; the surfaces stay above all other
windows and can take the keyboard exclusively."))

(defclass fullscreen-backend (backend)
  ()
  (:documentation "Cover outputs with fullscreen windows. This works with any
compositor, but other windows can appear above it and the keyboard focus is
not guaranteed."))

(defgeneric backend-name (backend)
  (:documentation "Return a short name for BACKEND, for the log."))

(defmethod backend-name ((backend layer-shell-backend))
  "layer-shell")

(defmethod backend-name ((backend fullscreen-backend))
  "fullscreen")

(defgeneric prepare-window (backend window &key monitor keyboard)
  (:documentation "Make WINDOW cover MONITOR using BACKEND. KEYBOARD is
:EXCLUSIVE or :NONE, and says whether WINDOW takes the keyboard. Call this
before WINDOW is presented. Return WINDOW."))

(defmethod prepare-window ((backend layer-shell-backend) window
                           &key monitor (keyboard :exclusive))
  (lsh:enable-layer-shell window :monitor monitor :keyboard keyboard))

(defmethod prepare-window ((backend fullscreen-backend) window
                           &key monitor keyboard)
  "The keyboard mode cannot be set here, so KEYBOARD is ignored."
  (declare (ignore keyboard))
  (if monitor
      (window-fullscreen-on-monitor window monitor)
      (window-fullscreen window))
  window)

(defun choose-backend ()
  "Return the best backend the compositor supports: layer-shell if it is
there, else fullscreen. Call this once the display is open."
  (if (lsh:layer-shell-supported-p)
      (make-instance 'layer-shell-backend)
      (make-instance 'fullscreen-backend)))
