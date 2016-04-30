;;;; package.lisp

;; utilities exports everything from its own file
(defpackage :utilities
  (:use #:cl :esrap :inlined-generic-function))

(defpackage :math
  (:use #:cl :utilities)
  (:shadow :vector))


