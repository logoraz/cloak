;;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;;; Copyright (C) 2026 Erik P Almaraz
;;;;
;;;; Licensed under the LGPL-2.1-only with the LLGPL preamble;
;;;; see the LICENSE file in this repository.

(defpackage #:cloak/tests/ui
  (:use #:cl #:5am)
  (:import-from #:cloak/tests/suite #:suite)
  (:local-nicknames (#:cfg #:cloak/base/config)
                    (#:thm #:cloak/ui/theme)
                    (#:frm #:cloak/ui/form))
  (:export #:ui-suite)
  (:documentation "Tests for the parts of the UI layer that need no display."))

(in-package #:cloak/tests/ui)


(def-suite ui-suite
  :description "Tests for code/ui."
  :in suite)

(in-suite ui-suite)


;;; theme

(defun config-from (text)
  "Load a config file holding TEXT and return the config."
  (uiop:with-temporary-file (:stream out :pathname path)
    (write-string text out)
    (finish-output out)
    (cfg:load-config path)))

(test hex-color-rgb-reads-hex-colors
      "Both hex forms give three integers."
      (is (equal '(46 52 64) (thm::hex-color-rgb "#2e3440")))
      (is (equal '(255 255 255) (thm::hex-color-rgb "#fff")))
      (is (equal '(17 34 51) (thm::hex-color-rgb "#123"))))

(test hex-color-rgb-refuses-others
      "Anything that is not #RGB or #RRGGBB gives nil."
      (is (null (thm::hex-color-rgb "red")))
      (is (null (thm::hex-color-rgb "#")))
      (is (null (thm::hex-color-rgb "#12")))
      (is (null (thm::hex-color-rgb "#gggggg")))
      (is (null (thm::hex-color-rgb nil))))

(test css-color-applies-opacity
      "A hex color becomes rgba with the opacity; others pass through."
      (is (string= "rgba(46, 52, 64, 0.50)"
                   (thm::css-color "#2e3440" 0.5)))
      (is (string= "rgba(255, 255, 255, 0.08)"
                   (thm::css-color "#fff" 0.08)))
      (is (string= "red" (thm::css-color "red" 0.5))))

(test escape-css-escapes-quotes-and-backslashes
      "Double quotes and backslashes get a backslash."
      (is (string= "a\\\"b\\\\c" (thm::escape-css "a\"b\\c")))
      (is (string= "plain" (thm::escape-css "plain"))))

(test css-rule-skips-nil-declarations
      "A rule lists its declarations and drops the nil ones."
      (is (string= (format nil ".a {~%  color: red;~%  margin: 0;~%}~%")
                   (thm::css-rule ".a" "color: red" nil "margin: 0"))))

(test wallpaper-declarations-by-fit
      "The fit picks the size; no path gives no declarations."
      (is (null (thm::wallpaper-declarations nil :cover)))
      (is (member "background-size: contain"
                  (thm::wallpaper-declarations "/w.png" :contain)
                  :test #'string=))
      (is (member "background-size: 100% 100%"
                  (thm::wallpaper-declarations "/w.png" :fill)
                  :test #'string=))
      (is (member "background-size: cover"
                  (thm::wallpaper-declarations "/w.png" :cover)
                  :test #'string=)))

(test theme-css-uses-the-config
      "The CSS holds the alpha tint, the border and the clock style."
      (let ((css (thm:theme-css
                  (config-from
                   "(:theme (:alpha 0.5 :border \"#112233\")
                     :clock (:color \"#445566\" :size \"20px\"))"))))
        (is (search "rgba(46, 52, 64, 0.50)" css))
        (is (search "border: 1px solid #112233" css))
        (is (search ".cloak-clock {" css))
        (is (search "color: #445566" css))
        (is (search "font-size: 20px" css))))

(test theme-css-colors-the-buttons
      "The CSS colors each button, the greeting and the error notice."
      (let ((css (thm:theme-css (config-from "()"))))
        (is (search ".cloak-form button.cloak-login" css))
        (is (search "background: #5e81ac" css))
        (is (search "background: #4c566a" css))
        (is (search "background: #ebcb8b" css))
        (is (search "background: #bf616a" css))
        (is (search "color: #d08770" css))
        (is (search "font-size: 150%" css))))

(test theme-css-wallpaper-is-optional
      "With no wallpaper path there is no background image."
      (is (null (search "background-image"
                        (thm:theme-css (config-from "()")))))
      (let ((css (thm:theme-css
                  (config-from "(:theme (:wallpaper (:path \"/w.png\")))"))))
        (is (search "url(\"file:///w.png\")" css))))


;;; clock

(defun clock-at (control hour minute &optional (second 0))
  "Format CONTROL for Sunday 25 October 2026 at HOUR:MINUTE:SECOND."
  (frm::format-clock control
                     (encode-universal-time second minute hour 25 10 2026)))

(test format-clock-24-hour
      "%H, %M and %S give two digits."
      (is (string= "18:07:05" (clock-at "%H:%M:%S" 18 7 5))))

(test format-clock-12-hour
      "%I and %p give the 12-hour clock, with midnight and noon as 12."
      (is (string= "06:07 PM" (clock-at "%I:%M %p" 18 7)))
      (is (string= "12:00 AM" (clock-at "%I:%M %p" 0 0)))
      (is (string= "12:30 PM" (clock-at "%I:%M %p" 12 30)))
      (is (string= "01:00 AM" (clock-at "%I:%M %p" 1 0))))

(test format-clock-date
      "The date directives give the year, month, day and names."
      (is (string= "2026-10-25 Sun Oct" (clock-at "%Y-%m-%d %a %b" 9 0))))

(test format-clock-literals
      "%% is a percent sign; other directives and a lone % are kept."
      (is (string= "100%" (clock-at "100%%" 9 0)))
      (is (string= "%Q" (clock-at "%Q" 9 0)))
      (is (string= "at 5%" (clock-at "at 5%" 9 0))))
