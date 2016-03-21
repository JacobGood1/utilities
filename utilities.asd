;;;; utilities.asd

(asdf:defsystem #:utilities
  :description "Describe utilities here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:infix-math
	       #:arrow-macros
	       #:esrap
	       #:closer-mop
	       #:inlined-generic-function
	       #:cl-parallel
	       #:cffi)
  :serial t
  :components ((:file "package")
	       (:file "utility-functions")
               (:file "utilities")))

