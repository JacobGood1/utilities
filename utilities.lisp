;;;; utilities.lisp

(in-package #:utilities)

;;; "utilities" goes here. Hacks and glory await!


 

  (defmacro set! 
      (&rest vars) 
    (cons 'progn 
	  (loop for v in (partition vars 2)  
	     collect `(setf ,@v))))

  (defmacro if-not (bool true &optional (false nil))
    `(if (not ,bool) ,true ,false))

  (defmacro fn (args &body body)
    `(lambda ,args ,@body))

  (defun ask (string)
    (princ string *query-io*)
    (read *query-io*))

  (defmacro def-generic (name args) 
    (fmakunbound name) ;makes sure that this can be redifined!
    `(defgeneric ,name 
	 ,(append args))) ; '(&key)
  
  


					;
					;(loop with x = 1 
					;      with new-list = (first test) 
					;      while (< x (length test)) 
					;            do (progn 
					;                 (setf new-list (append new-list (list (nth x test))))  
					;                 (setf x (1+ x)) 
					;      finally (return new-list))) 

  

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

   
(defmacro def-method ;;TODO make these work differently with mixins, need to extract data deps for with accessors and possible constructor will need luv
      (name args &rest body)
    (let ((unroll nil)
	  (find-objects (loop for elm in args when (consp elm) collect elm)))
      (if (not (nil? find-objects)) 
	  (setf unroll (list (nest-with-accessors (place-method-at-the-end-of-with-accessors (make-accessor-args find-objects) 
											       body))))
	  (setf unroll body))
      `(defmethod ,name 
	         ,args 
	         ,@unroll)))

(defmacro def-class
    (name &key extends slots constructor super-args)
  
  (setf extends (append extends '(printer-base)))
  
  (let ((def-method-code nil))
    (if constructor
	(if (eq (first constructor) 'lambda)
	    nil
	    (error "constructor must be a lambda expression")))
    
    (if slots
	(attach class-slots 
		name 
		(remove-duplicates (append (combine-slots extends) 
					   (grab-slots slots)))))
    
					;check for mixin dependency problems
    (let ((missing-deps (make-hash-table))
	  (mixins-that-need-deps '()))
      
      (loop for mixin in extends 
	 when (gethash mixin *mixins*)
	 do (loop for dep in (fetch mixin-dependencies mixin)
	       do (if-not (in? (slots-of name) dep) 
			  (if (gethash mixin missing-deps)
			      (setf (gethash mixin missing-deps) (remove-duplicates (cons dep (gethash mixin missing-deps))))
			      (progn
				(setf mixins-that-need-deps (cons mixin mixins-that-need-deps)) 
				(setf (gethash mixin missing-deps) (cons dep '())))))))
      
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
						       (fetch missing-deps mixin)
						       " which is absent from: "
						       name))) 
		    finally (return error-string)))))
    
    (let* ((constructor-args (first (rest constructor)))
	   (constructor-requirements constructor-args))
      
      (loop for arg in constructor-args
	 do (loop for slot in slots
		when (eq slot arg)
	       do (error (format nil "The slot: ~A and constructor arguement: ~A should not have the same symbol!" slot arg))))
      
      
      
      (loop for class in extends 
	 do (loop for arg in constructor-args 
		 do (if (in? (slots-of class) arg)
			(warn (to-string "slot: " arg " belonging to class: " class " is being shadowed by constructor arg: " arg)))))
      
      (setf def-method-code 
	    `(defmethod initialize-instance
		 :after
	       ((obj ,name) &key ,@constructor-args)
	       (with-accessors ,(loop for (i _) in (grab-slots-with-accessors (slots-of name))
				   collect (list i i)) 
		   obj 
		 ,@(cddr constructor)
		 (when (next-method-p)
		   ,(if super-args `(call-next-method obj ,@super-args) `(call-next-method obj))))))

      `(progn
	 
	 (defclass ,name 
	     ,extends 
	   ,(append (gen-slots slots) `((name :initform ',name :reader name :allocation :class))))
	 
	 ,def-method-code
	 
	 ',(setf constructor-args (append constructor-args (slots-of name)))
	 
	 (defmacro ,name
	       (&key ,@constructor-args)
	   
	   (if (fetch *mixins* ',name)
	       (error "This class is a mixin, they cannot be instantiated directly"))
	   
	     (if ',constructor-requirements
		 (if-not (and ,@constructor-requirements)
			 (error (format nil "Please provide the following arguements for the constructor: ~A"
					(loop for arg in ',constructor-requirements
					   collect (loop for a in ',constructor-args
						      when (eq a arg)
						      return arg))))))
	     `(make-instance ',',name
			     ,@(flatten
				(loop
				   for (arg value) in (partition
						       (interleave ',constructor-args
								   (list ,@constructor-args))
						       2)
				   when (not (nil? value)) 
				   collect (list (to-keyword arg) value)))))))))





(defmacro def-mixin 
    (name &key extends slots constructor dependencies super-args)
  
  (let ((data-deps '()))
    
    (loop for dep in dependencies
       do (handler-case (progn (find-class dep)
				 (setf data-deps (append data-deps (slots-of dep))))
	      (simple-error () (setf data-deps (append data-deps `(,dep))))))
      
      (attach *mixins* 
	      name
	      name)
      
      (if dependencies 
	  (attach mixin-dependencies 
		  name
		  (remove-duplicates data-deps)))
     
      `(def-class
	   ,name
	   :extends ,extends
	   :slots ,slots
	   :constructor ,constructor
	   :super-args ,super-args)))

  (defmacro make 
      (obj &rest args)
    (if (fetch *mixins* obj)
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

  (def-method fetch ((coll hash-table) k)
    (gethash k coll))

(defmacro -=
    (var value)
  `(setf ,var (- ,var ,value)))
(defmacro +=
    (var value)
  `(setf ,var (+ ,var ,value)))

(defmacro ->
    (init &body exps)
  `(arrow-macros:-> ,init ,@exps))


 ;export every symbol in the current package except the ones listed package
(defmacro export-all-symbols-except
    (&rest private-symbol-list)
  `(let ((pack (find-package *PACKAGE*)))
    (do-all-symbols (sym pack) (when (and (not (in? ',private-symbol-list sym))
					  (eql (symbol-package sym) pack))
				 (export sym)))))
 

(export-all-symbols-except nil)
  
