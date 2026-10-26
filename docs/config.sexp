;;; cloak.sexp: an example of every setting.
;;;
;;; Copy this file to /etc/greetd/cloak.sexp, or point cloak at it with
;;; --config. The file is plain data and is never evaluated: no quote
;;; marks, and one list holding everything. Every setting is optional;
;;; a missing or invalid one uses the default shown in its comment.
;;; Paths must be absolute; ~ is not expanded.

(
 ;; The user that is selected first. The avatar is the picture for this
 ;; user; every other user shows the initials of their login name.
 :user (:name "locutus"                      ; default: none
        :avatar "/home/locutus/avatar.png")  ; default: none

 :theme (;; Colors are hex: #rgb or #rrggbb.
         :background "#2e3440"       ; window and login box color
         :foreground "#e5e9f0"       ; text color
         :border "#81a1c1"           ; login box border

         ;; Opacity of the login box only, from 0 (clear) to 1 (solid).
         :alpha 0.85

         ;; Extra CSS file, applied on top of the theme. The --style
         ;; flag is applied on top of this one.
         :css "/etc/greetd/cloak.css"        ; default: none

         :cursor "Bibata-Modern-Classic"     ; cursor theme name
         :font "JetBrains Mono"              ; font family

         ;; With no path the window shows the background color alone.
         ;; :fit is :cover, :contain or :fill.
         :wallpaper (:path "/usr/share/backgrounds/wallpaper.png"
                     :fit :cover))

 ;; The desktop session selected first, by its name in the menu, such as
 ;; "Sway". Sessions come from the .desktop files of the system, so none
 ;; are listed here. After a login, the last session used is selected.
 :session (:default "Sway"                   ; default: none
           ;; A command, as a list of strings, to start after every login
           ;; instead of the command of the session picked in the menu.
           ;; When it is set the session menu is hidden.
           :command ("/path/to/start-wm"))   ; default: none

 ;; :primary is the monitor that shows the login form, named as sway
 ;; names it (swaymsg -t get_outputs). With none, or a name that is not
 ;; plugged in, the first monitor is used. Every other monitor shows the
 ;; wallpaper only. :position is :center, :left or :right, and is not
 ;; used yet.
 :layout (:primary "eDP-1"                   ; default: none
          :position :center)

 ;; :format uses these directives: %H %I %M %S %p %Y %m %d %a %b and %%.
 ;; The default is 12-hour; "%H:%M" gives 24-hour. :size is a CSS
 ;; font size.
 :clock (:format "%I:%M %p"
         :color "#81a1c1"
         :size "48px")

 ;; Text above the clock.
 :greeting "Welcome"

 ;; Each command is a list of strings, run when its button is clicked.
 :commands (:reboot ("loginctl" "reboot")
            :poweroff ("loginctl" "poweroff")))
