;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:cloak/ui/layer-shell
  (:use #:cl)
  (:export #:layer-shell-supported-p
           #:enable-layer-shell)
  (:documentation "Layer-shell windows for the greeter, one per output."))

(in-package #:cloak/ui/layer-shell)


;;; Code:

(defvar *namespace* nil
  "The Gtk4LayerShell namespace, loaded on first use.")

(defparameter *layers*
  '((:background . 0) (:bottom . 1) (:top . 2) (:overlay . 3))
  "The layer-shell layers and their values.")

(defparameter *edges*
  '((:left . 0) (:right . 1) (:top . 2) (:bottom . 3))
  "The layer-shell edges and their values.")

(defparameter *keyboard-modes*
  '((:none . 0) (:exclusive . 1) (:on-demand . 2))
  "The layer-shell keyboard modes and their values.")

(defun enum-value (table key)
  "Return the number for KEY in TABLE, or signal an error if it has none."
  (let ((entry (assoc key table)))
    (if entry
        (cdr entry)
        (error "Unknown layer-shell value ~S" key))))

(defun namespace ()
  "Return the Gtk4LayerShell namespace, loading it the first time."
  (or *namespace*
      (setf *namespace* (gir:require-namespace "Gtk4LayerShell" "1.0"))))

(defun layer-shell-supported-p ()
  "Return true if the compositor supports the layer-shell protocol."
  (let ((ns (namespace)))
    (gir:invoke (ns "is_supported"))))

(defun enable-layer-shell (window &key monitor (layer :overlay)
                                       (keyboard :exclusive))
  "Make WINDOW a layer-shell surface that covers its whole output, on LAYER,
with KEYBOARD as its keyboard mode. MONITOR is the GdkMonitor to cover; with
none, the compositor chooses. Call this before WINDOW is presented."
  (let ((ns (namespace)))
    (gir:invoke (ns "init_for_window") window)
    (when monitor
      (gir:invoke (ns "set_monitor") window monitor))
    (gir:invoke (ns "set_namespace") window "cloak")
    (gir:invoke (ns "set_layer") window (enum-value *layers* layer))
    (gir:invoke (ns "set_keyboard_mode") window
                (enum-value *keyboard-modes* keyboard))
    (gir:invoke (ns "set_exclusive_zone") window -1)
    (dolist (edge '(:top :bottom :left :right))
      (gir:invoke (ns "set_anchor") window (enum-value *edges* edge) t))
    window))
