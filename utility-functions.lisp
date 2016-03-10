(in-package #:utilities)

 (infix-math:use-infix-math)

  

(declaim (inline nil?))
(defun nil?
    (obj)
  (eq nil obj))

(defun in?
    (list item)
  (cond ((eq (first list) item) t)
	((not list) nil)
	(t (in? (rest list) item))))

(defun filter
    (f list)
  (loop for i in list
     when (funcall f i)
     collect i))

  ;TODO need to make this a multimethod
  (defun interleave (a b)
  (flet ((nil-pad (list on-list)
          (append list (make-list (max 0 (- (length on-list) (length list)))))))
    (loop for x in (nil-pad a b)
          for y in (nil-pad b a)
          append (list x y))))

  (defun insert-after (lst index newelt)
    (push newelt (cdr (nthcdr index lst))) 
    lst)

;(fmakunbound 'imap) just in case you need to redifine a generic fn

  (defgeneric imap (fn coll))

  (defmethod imap ((fn function) (coll vector))
    (loop
       with length = (length coll)
       with array = (make-array length)
       for i upto (1- length)
       do (setf (aref array i) (funcall fn (aref coll i)))
       finally (return array)))

(defgeneric for-each (coll fn))

  (defmethod for-each ((coll vector) (fn function))
    (loop
       with length = (length coll)
       for i upto (1- length)
       do (setf (aref coll i) (funcall fn (aref coll i)))
       finally (return coll)))

  (defun flatten (x)
    (labels ((rec (x acc)
	       (cond ((null x) acc)
		     ((atom x) (cons x acc))
		     (t (rec
			 (car x)
			 (rec (cdr x) acc))))))
      (rec x nil)))

(defun to-string (&rest args)
    (apply #'concatenate 
	   'string 
	   (loop for i in args 
	      collect  (if (stringp i) 
			   i 
			   (write-to-string i)))))
  
  (defun to-symbol (str) (intern (string-upcase str)))

  (defun to-keyword (name) 
    (values (intern (string-upcase name) "KEYWORD")))
  
  (defun partition (source n)
    (if (zerop n) (error "zero length"))
    (labels ((rec (source acc)
	       (let ((rest (nthcdr n source)))
		 (if (consp rest)
		     (rec rest (cons
				(subseq source 0 n)
				acc))
		     (nreverse
		      (cons source acc))))))
      (if source (rec source nil) nil)))
  

(defun slots-of (obj) (gethash obj *class-slots*))

 ;TODO need to make first work on every seqable structure!
(defgeneric 1st (seq))

  (defmethod 1st ((str string))
    (char str 0))

  (defmethod 1st ((lst cons))
    (funcall lisp-first lst))

  (defmethod 1st ((coll null))
    (funcall lisp-first coll))

  (defmethod 1st ((coll vector))
    (if-not (= (length coll)
	       0)
	    (aref coll 0)
	    nil))


(defparameter *class-slots* (make-hash-table))
(defparameter *mixin-dependencies* (make-hash-table))
(defparameter *mixins* (make-hash-table))

(defun gen-slots (slots)
  (restart-case 
      (if (not (evenp (length slots)))
	  (error "Not all slots have been assigned a value!")
	  (loop for (a b) in (partition slots 2)
	     collect `(,a
		       :initarg  ,(to-keyword a) 
		       :initform ,b 
		       :accessor ,a)))
    (use-value (value)
      :report "Create a new slot list"
      :interactive (lambda () (list (ask "Value: ")))
      (gen-slots value))))

(defun append-obj-slots 
      (obj-list)
  (partition
   (flatten (loop for (var obj) in obj-list 
	       collect (if (eq obj nil) 
			   nil 
			   (loop for slot in (slots-of obj) 
			      collect (list (to-symbol (to-string var "-" slot)) 
					    slot))))) 
   2))

(defun create-with-accessor-for-obj 
      (obj) 
    `(with-accessors ,(loop for slot in (slots-of obj) 
			 collect (list (to-symbol (to-string obj "-" slot)) 
				       slot)) 
	 ,obj))

  (defun create-with-accessor-for-overrides-and-setters 
      (obj) 
    `(with-accessors ,(loop for slot in (slots-of obj) 
			 collect (list slot 
				       slot)) 
	 ,obj))

(defun make-accessor-args
      (obj-list)
    "takes an argument list that would normally
   go to the defmethod function...
   '((point1 one) (point2 two))
   it then obtains the slots of the objects that defmethod dispatches on
   assuming one and two are points...
   ((x y) (x y))
   it then takes these slots and matches them with their object like so...
   ((WITH-ACCESSORS ((POINT1-X X) (POINT1-Y Y)) ONE)
    (WITH-ACCESSORS ((POINT2-X X) (POINT2-Y Y)) TWO) NIL)"
    (loop for (var obj) in obj-list 
       collect (cond ((eq obj nil)
		      nil)
		     ((gethash obj *mixins*)
		      `(with-accessors
			     ,(loop for slot in (gethash obj mixin-dependencies) 
				 collect (list (to-symbol (to-string var "-" slot)) 
					       slot)) 
			     ,var))
		     (:default
		      `(with-accessors ,(loop for slot in (slots-of obj) 
					   collect (list (to-symbol (to-string var "-" slot)) 
							 slot)) 
			   ,var)))))

(defun place-method-at-the-end-of-with-accessors
      (with-accessor-list body)
    (let* ((code  (filter (lambda (x) (not (eq x nil))) 
			  with-accessor-list))
	   (last-code (loop with last-code = (first (last code))
			 for code in body 
			 do (setf last-code (append last-code (list code))) 
			 finally (return last-code)))
	   (code (append (butlast code) 
			 (list last-code)))) 
      code))

  (defun nest-with-accessors 
      (with-accessor-list)
    "takes a list such as the one above ^ ...
  ((WITH-ACCESSORS ((POINT1-X X) (POINT1-Y Y)) ONE)
    (WITH-ACCESSORS ((POINT2-X X) (POINT2-Y Y)) TWO) NIL)
  and nests the with-accessors together like so...
  (WITH-ACCESSORS ((POINT1-X X) (POINT1-Y Y))
                ONE
                (WITH-ACCESSORS ((POINT2-X X) (POINT2-Y Y)) TWO (NIL)))
  the nil at the end does not matter..." 
    (if (not (= (length with-accessor-list) 1)) 
	(append (first with-accessor-list) 
		(list (nest-with-accessors (rest with-accessor-list))))
	(first with-accessor-list)))



(defun combine-slots (classes)
    (remove-duplicates 
     (flatten 
      (loop for class in classes collect (slots-of class)))))

  (defun grab-slots (slots)
    (loop for (a b) in (partition slots 2) collect a))

  (defun grab-slots-with-accessors (slots)
    (loop for slot in slots collect (list slot slot)))


;printer-base is an object that ecery object derives from
  ;it will ensure that all object created by def-class are printed much readable
  (defclass printer-base () ())

;print-object makes all objects of the type printer-base print more readable
  (defmethod print-object ((object printer-base) stream)
    (format stream
	    "Object: ~A :slots ~A"
	    (name object)
	    (interleave (slots-of (name object))
			(loop for slot in (slots-of (name object))
			   collect (funcall slot object)))))

(defgeneric attach (coll &optional key value))

  (defmethod attach ((coll hash-table) &optional key value)
    (if-not (and key value)
	    (error "A value and key must be supplied to the method attach when invoked on a map")
	    (setf (gethash key coll) 
		  value)))
  
  (defmethod attach ((coll vector) &optional value key)
    (cond ((and value key)
	   (error "attach takes 2 arguements when applied to vectors, not 3"))
	  ((nil? value)
	   (error "A value must be supplied to the method attach"))
	  (:default
	   (vector-push-extend value coll))))

  (defmethod attach ((coll cons) &optional value pos)
    (cond ((nil? value) 
	   (error "A value must be supplied to the method attach"))
	  ((and value pos)
	   (push value (cdr (nthcdr pos coll)))
	   coll)
	  (:default
	   (push value (cdr (nthcdr 0 coll)))
	   coll)))





  
