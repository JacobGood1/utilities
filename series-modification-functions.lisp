(in-package #:utilities)
;test


(def-class rebol-block 
    :slots ((container (make-array 1 :adjustable t :fill-pointer 0))
	    (head 0)
	    (tail 0)
	    (current-position 0)))

(def-method at
            rebol-block
            ((pos integer))
	    (setf current-position pos)
	    rebol-block)

;test end
