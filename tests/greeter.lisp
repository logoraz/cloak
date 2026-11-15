(defpackage #:cloak/tests/greeter
  (:use #:cl #:5am)
  (:import-from #:cloak/tests/suite #:suite)
  (:local-nicknames (#:clk #:cloak))
  (:export #:greeter-suite)
  (:documentation "Tests for the greeter layer."))

(in-package #:cloak/tests/greeter)


(def-suite greeter-suite
  :description "Tests for code/main.lisp."
  :in suite)

(in-suite greeter-suite)


;;; parse-args

(test parse-args-defaults
      "With no arguments the options hold the defaults."
      (multiple-value-bind (options unknown) (clk::parse-args '())
        (is (string= clk::*default-config* (getf options :config)))
        (is (string= clk::*default-log* (getf options :log)))
        (is (null (getf options :style)))
        (is (null unknown))))

(test parse-args-reads-flag-values
      "Each flag takes the argument after it."
      (multiple-value-bind (options unknown)
          (clk::parse-args '("--config" "/a" "--style" "/b.css"
                             "--log" "/c"))
        (is (string= "/a" (getf options :config)))
        (is (string= "/b.css" (getf options :style)))
        (is (string= "/c" (getf options :log)))
        (is (null unknown))))

(test parse-args-keeps-unknown-arguments
      "Arguments that are not flags are returned, in order."
      (multiple-value-bind (options unknown)
          (clk::parse-args '("--bogus" "--log" "/c" "extra"))
        (is (string= "/c" (getf options :log)))
        (is (equal '("--bogus" "extra") unknown))))

(test parse-args-flag-without-value
      "A flag at the end with no value is unknown; the default stays."
      (multiple-value-bind (options unknown)
          (clk::parse-args '("--log"))
        (is (string= clk::*default-log* (getf options :log)))
        (is (equal '("--log") unknown))))

(test parse-args-last-flag-wins
      "A flag given twice takes its last value."
      (let ((options (clk::parse-args '("--log" "/a" "--log" "/b"))))
        (is (string= "/b" (getf options :log)))))
