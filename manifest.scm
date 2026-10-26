;; Guix Development Package Manifest
;;
;; Run:
;;
;; guix shell -m manifest.scm
;; export LD_LIBRARY_PATH="$GUIX_ENVIRONMENT/lib:$LD_LIBRARY_PATH"
;; sbcl
;;

(use-modules (gnu packages))

(specifications->manifest
  (list
    "sbcl"                   ; Common Lisp implementation

    ;; CFFI/FFI Substrate
    "libffi"                 ; runtime: CFFI's foreign-call backend

    ;; GTK4 / GObject Introspection UI stack
    "gtk"                    ; runtime: GTK4 C library
    "glib"
    "gobject-introspection"
    "gtk4-layer-shell"        ; runtime: Wayland layer-shell protocol support
    "libadwaita"

    ;; Guix-side authoring (guix.scm, guix/cloak/services.scm)
    "guile"
    "guix"))
