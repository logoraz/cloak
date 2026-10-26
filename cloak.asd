(defsystem "cloak"
  :description "A configurable GTK4 greetd greeter in Common Lisp."
  :author "Erik P Almaraz"
  :license "LGPL-2.1-only WITH LLGPL"
  :version (:read-file-form "data/version.sexp" :at (0 1))
  :depends-on ("sb-bsd-sockets"
               "sb-posix"
               "com.inuoe.jzon"
               "cl-gtk4"
               "cl-gdk4"
               "cl-gtk4.adw")
  :components ((:module "lib"
                :pathname "code/lib"
                :components ((:file "jzon-message")
                             (:file "logging")))
               (:module "base"
                :pathname "code/base"
                :depends-on ("lib")
                :components ((:file "greetd-ipc")
                             (:file "login" :depends-on ("greetd-ipc"))
                             (:file "config")
                             (:file "sessions")
                             (:file "users")
                             (:file "state")))
               (:module "ui"
                :pathname "code/ui"
                :depends-on ("lib" "base")
                :components ((:file "layer-shell")
                             (:file "backend" :depends-on ("layer-shell"))
                             (:file "outputs" :depends-on ("backend"))
                             (:file "theme")
                             (:file "form" :depends-on ("theme"))))
               (:module "greeter"
                :pathname "code"
                :depends-on ("lib" "base" "ui")
                :components ((:file "cloak"))))
  :in-order-to ((test-op (test-op "cloak/tests")))
  :build-operation "program-op"
  :build-pathname "cloak"
  :entry-point "cloak:main")


;;; Subsystems

(defsystem "cloak/tests"
  :description "Unit Tests."
  :depends-on ("cloak" "fiveam")
  :components
  ((:module "tests"
    :serial t
    :components
    ((:file "suite")
     (:file "lib")
     (:file "base")
     (:file "ui")
     (:file "greeter"))))
  :perform (test-op (o c) (symbol-call :fiveam :run!
                                       (find-symbol "SUITE"
                                                    :cloak/tests/suite))))
