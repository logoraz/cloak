(defpackage #:cloak/ui/theme
  (:use #:cl)
  (:local-nicknames (#:cfg #:cloak/base/config))
  (:export #:theme-css
           #:apply-theme
           #:style-window
           #:make-cover)
  (:documentation "Look of the greeter: CSS made from the config, the cursor
theme, and the wallpaper-only window shown on outputs without the form.

The CSS uses these style classes. A window that covers an output has
cloak-window, the login box has cloak-form, and the greeting in it has
cloak-greeting. The clock has cloak-clock and the notice under the form
has cloak-notice, with cloak-error added for an error message. The Login
and Cancel buttons have cloak-login and cloak-cancel; the box of the power
buttons has cloak-power, and its buttons cloak-reboot and cloak-poweroff."))

(in-package #:cloak/ui/theme)


;;; Code:

(defparameter *priorities* '(:theme 600 :config 700 :style 800)
  "The GTK style provider priority of each source of CSS. A higher number
wins, so the file named by --style overrides the config's :css file, which
overrides the theme made from the config.")

(defparameter *error-color* "#d08770"
  "The color of error messages, nord orange.")

(defparameter *light-text* "#eceff4"
  "The text color on the dark buttons, the brightest white of nord.")

(defparameter *dark-text* "#2e3440"
  "The text color on the light buttons, the darkest color of nord.")

(defparameter *login-color* "#5e81ac"
  "The background of the Login button, nord blue.")

(defparameter *cancel-color* "#4c566a"
  "The background of the Cancel button, a nord gray.")

(defparameter *reboot-color* "#ebcb8b"
  "The background of the Reboot button, nord yellow.")

(defparameter *poweroff-color* "#bf616a"
  "The background of the Power off button, nord red.")

(defun escape-css (string)
  "Return STRING with its backslashes and double quotes escaped, for use
inside a CSS string."
  (with-output-to-string (out)
    (loop :for char :across string
          :do (when (member char '(#\\ #\"))
                (write-char #\\ out))
              (write-char char out))))

(defun hex-color-rgb (color)
  "Return the red, green and blue of the hex COLOR, as #RGB or #RRGGBB, as
a list of three integers. Return nil if COLOR is not a hex color."
  (let ((digits (and (stringp color)
                     (> (length color) 1)
                     (char= #\# (char color 0))
                     (subseq color 1))))
    (when (and digits (every (lambda (char) (digit-char-p char 16)) digits))
      (case (length digits)
        (3 (loop :for char :across digits
                 :collect (* 17 (digit-char-p char 16))))
        (6 (loop :for start :from 0 :below 6 :by 2
                 :collect (parse-integer digits :start start
                                                :end (+ start 2)
                                                :radix 16)))))))

(defun css-color (color alpha)
  "Return COLOR as a CSS color with the opacity ALPHA. A color that is not
hex is returned as it is, since CSS cannot give it an opacity."
  (let ((rgb (hex-color-rgb color)))
    (if rgb
        (format nil "rgba(~{~D~^, ~}, ~,2F)" rgb alpha)
        color)))

(defun css-url (path)
  "Return a CSS url() for the file at PATH."
  (format nil "url(\"file://~A\")" (escape-css path)))

(defun css-rule (selector &rest declarations)
  "Return the CSS rule for SELECTOR holding the DECLARATIONS that are not
nil. Each declaration is a string such as \"color: red\"."
  (format nil "~A {~%~{  ~A;~%~}}~%" selector (remove nil declarations)))

(defun wallpaper-declarations (path fit)
  "Return the CSS declarations that draw the wallpaper at PATH, scaled
by FIT, :cover, :contain or :fill. Return nil if PATH is nil."
  (when path
    (list (format nil "background-image: ~A" (css-url path))
          (format nil "background-size: ~A"
                  (case fit
                    (:contain "contain")
                    (:fill "100% 100%")
                    (t "cover")))
          "background-position: center"
          "background-repeat: no-repeat")))

(defun shade (color factor)
  "Return the hex COLOR with each of its channels multiplied by FACTOR, a
number from 0 to 1, as #RRGGBB. A color that is not hex is returned as it
is."
  (let ((rgb (hex-color-rgb color)))
    (if rgb
        (format nil "#~{~2,'0X~}"
                (mapcar (lambda (channel) (round (* channel factor))) rgb))
        color)))

(defun button-rule (selector background text)
  "Return the CSS rules that give the buttons matching SELECTOR the color
BACKGROUND and the text color TEXT. The color is a little darker under the
pointer and darker still while pressed."
  (concatenate
   'string
   (css-rule selector
             (format nil "background: ~A" background)
             (format nil "color: ~A" text)
             "transition: background 150ms")
   (css-rule (format nil "~A:hover" selector)
             (format nil "background: ~A" (shade background 0.8)))
   (css-rule (format nil "~A:active" selector)
             (format nil "background: ~A" (shade background 0.6)))))

(defun theme-css (config)
  "Return the CSS for the theme in CONFIG, as a string."
  (let ((background (cfg:config-ref config :theme :background))
        (foreground (cfg:config-ref config :theme :foreground))
        (border (cfg:config-ref config :theme :border))
        (alpha (cfg:config-ref config :theme :alpha))
        (font (cfg:config-ref config :theme :font))
        (clock-color (cfg:config-ref config :clock :color))
        (clock-size (cfg:config-ref config :clock :size))
        (path (cfg:config-ref config :theme :wallpaper :path))
        (fit (cfg:config-ref config :theme :wallpaper :fit)))
    (concatenate
     'string
     (apply #'css-rule ".cloak-window"
            (format nil "background-color: ~A" background)
            (wallpaper-declarations path fit))
     (css-rule ".cloak-form"
               (format nil "background-color: ~A" (css-color background alpha))
               (format nil "border: 1px solid ~A" border)
               "border-radius: 10px"
               "padding: 50px"
               "min-width: 400px")
     (css-rule ".cloak-form label, .cloak-form entry, .cloak-form button"
               (format nil "color: ~A" foreground)
               (format nil "font-family: \"~A\"" (escape-css font)))
     (css-rule ".cloak-form entry"
               (format nil "background-color: ~A" (css-color foreground 0.08)))
     (css-rule ".cloak-form .cloak-greeting"
               "font-size: 150%")
     (button-rule ".cloak-form button.cloak-login"
                  *login-color* *light-text*)
     (button-rule ".cloak-form button.cloak-cancel"
                  *cancel-color* *light-text*)
     (css-rule ".cloak-clock"
               (format nil "color: ~A" clock-color)
               (format nil "font-size: ~A" clock-size)
               (format nil "font-family: \"~A\"" (escape-css font)))
     (css-rule ".cloak-notice"
               (format nil "color: ~A" foreground)
               (format nil "font-family: \"~A\"" (escape-css font)))
     (css-rule ".cloak-error"
               (format nil "color: ~A" *error-color*)
               "font-size: 150%")
     (css-rule ".cloak-power button"
               (format nil "font-family: \"~A\"" (escape-css font))
               "min-width: 8em")
     (button-rule ".cloak-power button.cloak-reboot"
                  *reboot-color* *dark-text*)
     (button-rule ".cloak-power button.cloak-poweroff"
                  *poweroff-color* *light-text*))))

(defun add-css (css priority)
  "Add the CSS text to the default display, at the style PRIORITY."
  (let ((provider (gtk:make-css-provider)))
    (gtk:css-provider-load-from-string provider css)
    (gtk:style-context-add-provider-for-display
     (gdk:display-default) provider priority)))

(defun read-css-file (path)
  "Return the text of the CSS file at PATH. If it cannot be read, warn and
return nil."
  (handler-case (uiop:read-file-string path)
    (error (condition)
      (warn "css: cannot read ~A: ~A" path condition)
      nil)))

(defun apply-css (config style)
  "Add the CSS of the theme in CONFIG to the display, then the CSS file
named by the theme's :css, then the CSS file at STYLE. STYLE may be nil."
  (add-css (theme-css config) (getf *priorities* :theme))
  (loop :for (path key) :in (list (list (cfg:config-ref config :theme :css)
                                        :config)
                                  (list style :style))
        :for css := (and path (read-css-file path))
        :when css
          :do (add-css css (getf *priorities* key))))

(defun apply-cursor (config)
  "Set the cursor theme of the default display from the theme in CONFIG."
  (setf (gtk:property (gtk:settings-default) "gtk-cursor-theme-name")
        (cfg:config-ref config :theme :cursor)))

(defun apply-scheme (config)
  "Set the dark or light look of GTK from the theme in CONFIG, through the
settings of the default display. Call this once the display is open."
  (setf (gtk:property (gtk:settings-default)
                      "gtk-application-prefer-dark-theme")
        (eq (cfg:config-ref config :theme :scheme) :dark)))

(defun apply-theme (config style)
  "Apply the theme in CONFIG: its CSS, the CSS file at STYLE, which may be
nil, its cursor theme, and its scheme. Call this once the display is open."
  (apply-css config style)
  (apply-cursor config)
  (apply-scheme config))

(defun style-window (window)
  "Give WINDOW the wallpaper and background of the theme. Return WINDOW."
  (gtk:widget-add-css-class window "cloak-window")
  window)

(defun make-cover (app)
  "Return a window of APP that shows only the wallpaper."
  (style-window (gtk:make-application-window :application app)))
