;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:cloak/base/config
  (:use #:cl)
  (:export #:load-config
           #:config-ref)
  (:documentation "Reads config.sexp as data, checked against a schema."))

(in-package #:cloak/base/config)


;;; Code:

(defparameter *schema*
  '(:user      (:name   (:type :string :default nil)
                :avatar (:type :path :default nil))
    :theme     (:background (:type :color :default "#2e3440")
                :foreground (:type :color :default "#e5e9f0")
                :border     (:type :color :default "#81a1c1")
                :alpha      (:type :number :min 0 :max 1 :default 0.85)
                :css        (:type :path :default nil)
                :cursor     (:type :string :default "Bibata-Modern-Classic")
                :font       (:type :string :default "JetBrains Mono")
                :wallpaper  (:path (:type :path :default nil)
                             :fit  (:type :enum
                                    :values (:cover :contain :fill)
                                    :default :cover)))
    :session   (:default (:type :string :default nil))
    :layout    (:position (:type :enum
                           :values (:center :left :right)
                           :default :center)
                :primary  (:type :string :default nil))
    :clock     (:format (:type :string :default "%I:%M %p")
                :color  (:type :color :default "#81a1c1")
                :size   (:type :string :default "48px"))
    :greeting  (:type :string :default "Welcome")
    :commands  (:reboot   (:type :command
                           :default ("loginctl" "reboot"))
                :poweroff (:type :command
                           :default ("loginctl" "poweroff"))))
  "The allowed settings, as a tree shaped like the config file. A leaf is
a plist with :type and :default, plus :values for :enum and optional :min
and :max for :number and :integer. No group may be named :type.")

(defun leaf-p (node)
  "True if NODE, a schema node, describes one setting rather than a group."
  (and (consp node) (eq (first node) :type)))

(defun plist-p (object)
  "True if OBJECT is a proper list of keyword and value pairs."
  (and (listp object)
       (null (cdr (last object)))
       (evenp (length object))
       (loop :for key :in object :by #'cddr
             :always (keywordp key))))

(defun warn-config (path control &rest args)
  "Warn about a config problem. PATH is the list of keys leading to it,
or nil for the file as a whole; CONTROL and ARGS are a format control and
its arguments."
  (warn "config~@[ ~{~(~S~)~^ ~}~]: ~?" path control args))

(defun valid-value-p (leaf value)
  "True if VALUE is acceptable for LEAF, a leaf of the schema."
  (let ((min (getf leaf :min))
        (max (getf leaf :max)))
    (ecase (getf leaf :type)
      ((:string :path :color) (stringp value))
      (:boolean (member value '(t nil)))
      ((:integer :number)
       (and (if (eq (getf leaf :type) :integer) (integerp value) (realp value))
            (or (null min) (>= value min))
            (or (null max) (<= value max))))
      (:enum (member value (getf leaf :values)))
      (:command (and (consp value)
                     (null (cdr (last value)))
                     (every #'stringp value))))))

(defun read-config-file (path)
  "Read the first form of the file at PATH as data and return it. Return
nil, after a warning, if the file cannot be read. Nothing is evaluated."
  (handler-case
      (with-open-file (in path :external-format :utf-8)
        (let ((*read-eval* nil)
              (*package* (find-package '#:cl-user))
              (eof (list nil)))
          (let ((form (read in nil nil)))
            (unless (eq (read in nil eof) eof)
              (warn-config nil "ignoring forms after the first in ~A" path))
            form)))
    (error (condition)
      (warn-config nil "cannot read ~A: ~A" path condition)
      nil)))

(defun warn-unknown-keys (schema user path)
  "Warn about each key of the plist USER that SCHEMA, a group, lacks."
  (let ((known (loop :for (key) :on schema :by #'cddr :collect key)))
    (loop :for key :in user :by #'cddr
          :unless (member key known)
          :do (warn-config (append path (list key))
                           "unknown key, ignored"))))

(defun resolve-leaf (leaf value present-p path)
  "Return the value to use for LEAF. PRESENT-P says whether the user gave
VALUE. A missing or invalid value gives the default of LEAF."
  (let ((default (copy-tree (getf leaf :default))))
    (cond ((not present-p) default)
          ((valid-value-p leaf value) value)
          (t (warn-config path "invalid ~(~A~) value ~S; using ~S"
                          (getf leaf :type) value default)
             default))))

(defun walk-config (schema user path)
  "Return the plist for SCHEMA, a group, filled in from USER, the plist the
user wrote for it. PATH is the list of keys leading to the group, for
warnings. Missing and invalid values take defaults; unknown keys warn."
  (unless (plist-p user)
    (warn-config path "expected key and value pairs, got ~S; using defaults"
                 user)
    (setf user nil))
  (warn-unknown-keys schema user path)
  (loop :for (key child) :on schema :by #'cddr
        :for sub-path := (append path (list key))
        :append (multiple-value-bind (found value tail)
                    (get-properties user (list key))
                  (declare (ignore found))
                  (list key
                        (if (leaf-p child)
                            (resolve-leaf child value (and tail t) sub-path)
                            (walk-config child (and tail value) sub-path))))))

(defun load-config (path)
  "Read the config file at PATH and return the full config, a plist shaped
like the schema. A missing file or a bad value only warns; the defaults
are used instead."
  (walk-config *schema* (read-config-file path) nil))

(defun config-ref (config &rest keys)
  "Return the value in CONFIG at the key path KEYS, for example
(config-ref config :theme :alpha). Signal an error for a key the schema
lacks, which is a typo in the code."
  (loop :for key :in keys
        :do (multiple-value-bind (found value tail)
                (get-properties config (list key))
              (declare (ignore found))
              (unless tail
                (error "No config key ~S in path ~S" key keys))
              (setf config value)))
  config)
