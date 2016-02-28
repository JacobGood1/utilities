;;;; utilities.lisp

(in-package #:utilities)

;;; "utilities" goes here. Hacks and glory await!

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;soon as compiliation of utilities occurs use infix math in whatever lib
  ;invokes it
  (infix-math:use-infix-math)

  (defun insert-after (lst index newelt)
    (push newelt (cdr (nthcdr index lst))) 
    lst)

  (defmacro set! 
      (&rest vars) 
    (cons 'progn 
	  (loop for v in (partition vars 2)  
	     collect `(setf ,@v))))
  
  (defun nil? (val)
    (eq nil val))


  (defun in?    ;TODO will not work for all data structures need a better solution
      (coll val)
    (member val coll))

  (defun make-vector () 
    (make-array 5 :fill-pointer 0 :adjustable t))


  (defun filter (pred list) 
    (loop for elm in list 
       when (funcall pred elm) 
       collect elm))

  (defmacro if-not (bool true &optional (false nil))
    `(if (not ,bool) ,true ,false))

					;TODO need to make first work on every seqable structure!
  (defparameter lisp-first #'first)
  (defparameter lisp-map #'map)

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

  (defmacro fn (args &body body)
    `(lambda ,args ,@body))

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
  
  (defun ask (string)
    (princ string *query-io*)
    (read *query-io*))
  
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
  
  (defun slots-of (obj) (gethash obj class-slots))

  (defun gen-slots (name slots)
    (restart-case 
	(if (not (evenp (length slots)))
	    (error "Not all slots have been assigned a value!")
	    (loop for (a b) in (partition slots 2)
	       collect `(,(to-symbol (to-string name "-" a)) 
                          :initarg  ,a 
                          :initform ,b 
                          :accessor ,a)))
      (use-value (value)
	:report "Create a new slot list"
	:interactive (lambda () (list (ask "Value: ")))
	(gen-slots name value))))
  
  
  (defmacro def-generic (name args) 
    (fmakunbound name) ;makes sure that this can be redifined!
    `(defgeneric ,name 
	 ,(append args '(&key))))
  
  (defun append-obj-slots 
      (obj-list)
    (partition (flatten (loop for (var obj) in obj-list 
			   collect (if (eq obj nil) 
				       nil 
				       (loop for slot in (slots-of obj) 
					  collect (list (to-symbol (to-string var "-" slot)) 
							slot))))) 
	       2))


					;
					;(loop with x = 1 
					;      with new-list = (first test) 
					;      while (< x (length test)) 
					;            do (progn 
					;                 (setf new-list (append new-list (list (nth x test))))  
					;                 (setf x (1+ x)) 
					;      finally (return new-list))) 

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

  (defmacro override-getter 
      (name obj code)
    `(defmethod ,name 
	 (,(list obj obj)) 
       ,(reverse (cons code 
		       (reverse (create-with-accessor-for-overrides-and-setters obj))))))
  (defmacro override-setter 
      (name obj value code)
    `(defmethod (setf ,name) 
	 (,value ,(list obj obj)) 
       ,(reverse (cons code 
		       (reverse (create-with-accessor-for-overrides-and-setters obj))))))

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
       collect (if (eq obj nil) 
		   nil 
		   `(with-accessors ,(loop for slot in (slots-of obj) 
					collect (list (to-symbol (to-string var "-" slot)) 
						      slot)) 
			,var))))

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



  (defmacro def-method 
      (name args &rest body)
    (if (member '&key args)
	(setf formatted-args args)
	(setf formatted-args (append args '(&key))))
    (let ((args (loop for elm in formatted-args when (consp elm) collect elm)))
      (if (not (nil? args)) 
	  (setf unroll (list (nest-with-accessors (place-method-at-the-end-of-with-accessors (make-accessor-args args) 
											     body))))
	  (setf unroll body))
      `(defmethod ,name 
	   ,formatted-args 
	 ,@unroll)))



  (defparameter class-slots (make-hash-table))
  (defparameter class-dependencies (make-hash-table))
  (defparameter mixins (make-hash-table))

  (def-generic attach (coll))

  (def-method attach ((coll hash-table) &key key value)
    (setf (gethash key coll) 
	  value))
  (def-method attach ((coll vector) &key value) 
    (vector-push-extend value coll))

  (def-method attach ((coll cons) &key value)
    (reverse (cons value (reverse coll))))



  (defun combine-slots (classes)
    (remove-duplicates 
     (flatten 
      (loop for class in classes collect (slots-of class)))))

  (defun grab-slots (slots)
    (loop for (a b) in (partition slots 2) collect a))

  (defun grab-slots-with-accessors (slots)
    (loop for slot in slots collect (list slot slot)))

  (defmacro def-class
      (name &key extends slots constructor super-args) 
    (if constructor
	(if (eq (first constructor) 'lambda)
	    nil
	    (error "constructor must be a lambda expression")))

					;check for after keyword
    (if (eq (first (rest constructor)) :after)
	(setf after :after)
	(setf after nil))

    (attach class-slots 
	    :key name 
	    :value (remove-duplicates (append (combine-slots extends) 
					      (grab-slots slots))))
    
    (let ((missing-deps (make-hash-table))
	  (mixins-that-need-deps '()))
      (loop for mixin in extends 
	 when (fetch mixins mixin) 
	 do (loop for class in (fetch class-dependencies mixin)
	       do (loop for slot in (slots-of class) 
		     do (if-not (in? (slots-of name) slot) 
				(if (gethash mixin missing-deps)
				    (setf (gethash mixin missing-deps) (remove-duplicates (cons class (gethash mixin missing-deps))))
				    (progn
				      (setf mixins-that-need-deps (cons mixin mixins-that-need-deps)) 
				      (setf (gethash mixin missing-deps) (cons class '()))))))))
      (if mixins-that-need-deps
	  (error (loop with error-string = "" 
		    for mixin in mixins-that-need-deps 
		    do (setf error-string 
			     (concatenate 'string 
					  error-string 
					  (to-string "~%" 
						     "mixin: " 
						     mixin 
						     " depends on "
						     (fetch missing-deps mixin)))) 
		    finally (return error-string)))))

    (let* ((constructor-args (if after (first (rest (rest constructor))) (first (rest constructor)))))
	  (loop for class in extends 
	     do (loop for arg in constructor-args 
		   do (if (in? (slots-of class) arg)
			  (warn (to-string "slot: " arg " belonging to class: " class " is being shadowed by constructor arg: " arg)))))
	  (setf def-method-code 
		`(defmethod initialize-instance 
		     ((obj ,name) &key ,@constructor-args)
		   (with-accessors ,(grab-slots-with-accessors (slots-of name)) 
		       obj 
		     ,@(if after (rest (rest (rest constructor))) (rest (rest constructor)))
		     (when (next-method-p)
		       ,(if super-args `(call-next-method obj ,@super-args) `(call-next-method obj))))))
	  `(progn
	     (defclass ,name 
		       ,extends 
	               ,(gen-slots name (append slots `(name ',name))))
	     ,(if after (insert-after def-method-code 2 :after) def-method-code))))





  (defmacro mixin 
      (name &key extends slots constructor dependencies super-args)
    (attach mixins
	    :key name
	    :value name)
    (if dependencies 
	(attach class-dependencies 
		:key name
		:value dependencies))
    `(class ,name
	    :extends ,extends
	    :slots ,slots
	    :constructor ,constructor
	    :super-args ,super-args))

  (defmacro make 
      (obj &rest args)
    (if (fetch mixins obj)
	(error "Mixins cannot be instantiated directly"))
    `(make-instance ',obj ,@args))

					;(defmacro def-class (name extends slots &key constructor) 
					;    (setf slots (append slots `(name ,name)))
					;    (attach class-slots 
					;            :key name 
					;            :value (append (combine-slots extends) 
					;                           (grab-slots slots)))
					;    `(progn (defclass ,name 
					;                      ,extends 
					;                      ,(gen-slots name slots))
					;            (defmethod initialize-instance 
					;                       :after 
					;                       ((obj ,name) &key)
					;                       (with-accessors ,(grab-slots-with-accessors slots) obj 
					;                                       ,@constructor))))
					;THIS VERSION DOES NOT WORK TELL DR MEISTER!




					;(let ((counter 0))
					;    (flet ((run-code-with-state (code) 
					;       (progn (setf counter (1+ counter)) code)))
					;      (defmacro new (name &rest args)
					;        `(make-instance ',name 
					;            ,@(mapcar (lambda (x) 
					;            (if (evenp counter)
					;                (run-code-with-state `(quote ,x))
					;                (run-code-with-state x)))    
					;          args)))))




  (def-generic fetch (coll item-to-fetch))

  (def-method fetch ((coll hash-table) key)
    (gethash key coll))

  (defmacro -=
      (var value)
    `(setf ,var (- ,var ,value)))
  (defmacro +=
      (var value)
    `(setf ,var (+ ,var ,value)))
  )
