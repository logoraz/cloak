;;; SPDX-License-Identifier: LGPL-2.1-only WITH LLGPL
;;; Copyright (C) 2026 Erik P Almaraz

;;; Development build from this checkout.  The package itself is defined in
;;; guix/cloak/packages.scm; this file swaps in the local source.
;;;
;;;   guix build -L guix -f guix.scm
;;;   guix shell -L guix -D -f guix.scm

(use-modules (cloak packages)
             (guix git)
             (guix packages)
             (guix utils))

(package
  (inherit cloak)
  (version (string-append (package-version cloak) "-dev"))
  (source (local-file "." "cloak-checkout"
                      #:recursive? #t
                      #:select? (git-predicate
                                 (current-source-directory)))))
