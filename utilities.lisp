;;;; utilities.lisp

(in-package #:utilities)

;;; "utilities" goes here. Hacks and glory await!


 
(defmacro var 
      (&rest vars) 
    (cons 'progn 
	  (loop for v in (partition vars 2)  
	     collect `(defparameter ,@v))))

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


;;TODO cleanup overrides when you get better at lisp, there is too much redundant code
;;TODO should renaming be possible?
(defmacro override-getter 
    (obj method-name code &key re-name)
  (if re-name
      (progn
	(fmakunbound method-name)
	`(defmethod ,re-name 
	     (,(list obj obj)) 
	   ,(reverse (cons code 
			   (reverse (create-with-accessor-for-overrides-and-setters obj))))))
      `(defmethod ,method-name 
	   (,(list obj obj)) 
	 ,(reverse (cons code 
			 (reverse (create-with-accessor-for-overrides-and-setters obj)))))))


(var *formatted-code-string* "")

;TODO NO SYMZ EXPOERTED
;no exporting syms '(conc-list-to-fcs *formatted-code-string* word paren whitespace number random-sym)

(defun conc-list-to-fcs
    (word)
  (setf *formatted-code-string*
	(concatenate 'string
		     *formatted-code-string*
		     (loop
			with str = ""
			for char in word
			do (if (consp char)
			       ()
			       (setf str (concatenate 'string
						      str
						      (string char))))
			finally (return str)))))

(defrule word
    (+ (alpha-char-p character))
  (:lambda (word)
    (conc-list-to-fcs word)))

(defrule number
    (+ (or (or "1" "2" "3" "4" "5" "6" "7" "8" "9" "0") "." "/"))
  (:lambda (num)
    (conc-list-to-fcs num)))

(defrule paren
    (or #\( #\))
  (:lambda (p)
    (setf *formatted-code-string*
	  (concatenate 'string
		       *formatted-code-string*
		       (string p)))))

(defrule whitespace
	       (+ (or #\space #\tab #\newline))
	     (:lambda (ws)
	       (conc-list-to-fcs ws)))

(defrule random-sym
    character
  (:lambda (c)
    (setf *formatted-code-string*
	  (concatenate 'string
		       *formatted-code-string*
		       (string c)))))

(defrule anything-but-quote (not #\"))

(defrule string-rule
    (and #\" (+ anything-but-quote) #\")
  (:lambda (c)
    (loop
       for elm in c
       do (if (not (consp elm))
	      (setf *formatted-code-string*
		    (concatenate 'string
				 *formatted-code-string*
				 (to-string elm)))
	      (conc-list-to-fcs elm)))))

(defun swap-symbol
    (text word-to-replace word)
  (setf *formatted-code-string* "")
  (eval `(defrule swap-word ,(write-to-string word-to-replace :case :downcase)
	   (:lambda (w)
	     (declare (ignore w))
	     (setf *formatted-code-string*
		   (concatenate 'string
				*formatted-code-string*
				,(write-to-string word :case :downcase))))))
  (parse '(+ (or swap-word string-rule whitespace paren word number random-sym))
	 (write-to-string text :case :downcase))
  (read-from-string *formatted-code-string*))

;;available words: value
(defmacro override-setter 
    (obj method-name code)
  `(defmethod (setf ,method-name) 
       (value ,(list obj obj)) 
     ,(reverse (cons (swap-symbol code method-name `(slot-value ,obj ',method-name)) 
		     (reverse (create-with-accessor-for-overrides-and-setters obj))))
     ,obj))

   
(defmacro def-method 
    (name args &rest body)
  (let* ((unroll nil)
	 (objects (loop for elm in args
		     when (consp elm)
		     collect (progn
			       (finalize-class (second elm))
			       elm))))
    
   (if (not (nil? objects)) 
       (setf unroll (generate-with-accessors objects 
					     body))
       (setf unroll body))
   
   (if (eq (first unroll) 'with-accessors)
       (setf unroll (list unroll)))
       
    `(defmethod ,name 
	 ,args 
       ,@unroll)))

(def-generic fetch (coll item-to-fetch))

;  (def-method fetch ((coll hash-table) k)
;    (gethash k coll))

 ;;;!!! code for dependency initialization, conform it to the new system!

;(let ((missing-deps (make-hash-table))
;      (mixins-that-need-deps '()))
;  
;  (loop for mixin in extends 
;     when (gethash mixin *mixins*)
;     do (loop for dep in (fetch *mixin-dependencies* mixin)
;	   do (if-not (in? (slots-of name) dep) 
;		      (if (gethash mixin missing-deps)
;			  (setf (gethash mixin missing-deps) (remove-duplicates (cons dep (gethash mixin missing-deps))))
;			  (progn
;			    (setf mixins-that-need-deps (cons mixin mixins-that-need-deps)) 
;			    (setf (gethash mixin missing-deps) (cons dep '())))))))
; 
;  (if mixins-that-need-deps
;      (error (loop with error-string = "" 
;		for mixin in mixins-that-need-deps 
;		do (setf error-string 
;			 (concatenate 'string 
;				      error-string 
;				      (to-string "~%" 
;						 "mixin: " 
;						 mixin 
;						 " depends on "
;						 (fetch missing-deps mixin)
;						 " which is absent from: "
;						 name))) 
;		finally (return error-string)))))

;;temporarily creating a new implimentation
(defmacro def-class
    (name &key extends slots constructor super-args dependencies)

  (eval `(defclass ,name ,extends ,(loop for (slot value) in (partition slots 2) collect slot)))

  (closer-mop:finalize-inheritance (find-class name))

  (if (and (nil? slots)
	   (nil? extends))
      (warn (to-string name " has no slots...")))

  (if constructor
      (if (eq (first constructor) 'lambda)
	  nil
	  (error "constructor must be a lambda expression")))

  (if dependencies
      (setf (gethash name *class-dependencies*) dependencies))

  (setf extends (append extends '(printer-base)))

  (let* ((slots-and-values slots)
	 (slots (slots-of name))
	 (def-method-code nil)
	 (constructor-args (first (rest constructor)))
	 (constructor-requirements constructor-args)
	 (constructor-body (cddr constructor)))
    
    (loop for arg in constructor-args
       do (loop for slot in slots
	     when (eq slot arg)
	     do (error (format nil "The slot: ~A and constructor arguement: ~A should not have the same symbol!" slot arg))))

    (setf def-method-code (if constructor
			      `(defmethod initialize-instance
				   :after
				 ((obj ,name) &key ,@constructor-args)
				 (with-accessors ,(loop for i in slots
						     collect (list i i)) 
				     obj 
				   ,@constructor-body
				   (when (next-method-p)
				     ,(if super-args `(call-next-method obj ,@super-args) `(call-next-method obj)))))))
    
    `(progn
					
       (defclass ,name 
	   ,extends 
	 ,(append (gen-slots slots-and-values) `((name :initform ',name :reader name :allocation :class))))
       
       ,def-method-code
       
       ',(setf constructor-args (append constructor-args slots))

       (defmacro ,name
	   (&key ,@constructor-args)
	 
	 (if ',dependencies
	     (error "This class has dependencies, it cannot be instantiated directly"))
	 
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
			       collect (list (to-keyword arg) value))))))))

(defmacro -=
    (var value)
  `(setf ,var (- ,var ,value)))
(defmacro +=
    (var value)
  `(setf ,var (+ ,var ,value)))

(defmacro ->
    (init &body exps)
  `(arrow-macros:-> ,init ,@exps))

(defmacro set-slots
    (obj &rest slots-and-values)
  (append '(progn) (loop for (slot value) in (partition slots-and-values 2)
                         collect `(setf (,slot ,obj) ,value))))


(defmacro defun-fast
    (name args &rest code)
  (let ((unroll (generate-with-accessors args code)))
	(if (eq (first unroll) 'with-accessors)
	    (setf unroll (list unroll)))
	(multiple-value-bind (defun-args types-code)
	    (generate-type-information args)
	  `(progn (declaim (inline ,name))
		  (defun ,name
		      ,defun-args
		      (declare (optimize (speed 3) (safety 0)))
		      ,@types-code
		      ,@unroll)))))

;; TODO implement this
;; fast defmethods do not use accessors, are inlined, and they are not generic, use them only in speed
;; critical situations
(defmacro def-method-fast
    (name args &rest code)
  ;;TODO FINISH THIS!!11
  `(defun-fast ,name ,args ,@code))


 ;export every symbol in the current package except the ones listed package
(defmacro export-all-symbols-except
    (&rest private-symbol-list)
  `(let ((pack (find-package *PACKAGE*)))
    (do-all-symbols (sym pack) (when (and (not (in? ',private-symbol-list sym))
					  (eql (symbol-package sym) pack))
				 (export sym)))))

;hash-table-macro
(defun read-separator (str)
 (let
  ((*readtable* (copy-readtable *readtable* nil)))
  (set-macro-character #\, (lambda (stream char)
                            (declare (ignore char) (ignore stream))
                            'break))
  (read str nil)))

(set-macro-character #\{
 (lambda (str char)
  (declare (ignore char))
  (let
   ((*readtable* (copy-readtable *readtable* nil)))
   (set-macro-character #\} (lambda (stream char)
                             (declare (ignore char) (ignore stream))
                             'end))

   (let
    ((pairs (loop for key = (read str nil nil t)
                  for sep = (read str nil nil t)
                  for value = (read str nil nil t)
                  for end? = (read-separator str)
                  do (when (not (eql '=> sep)) (error "Expected =>, did not get"))
                  do (when (not (or (eql 'end end?) (eql 'break end?))) (error "Expected , or }"))
                  collect (list key value)
                  while (not (eql 'end end?))))
     (retn (gensym)))
    `(let
      ((,retn (make-hash-table :test #'equal)))
      ,@(mapcar
         (lambda (pair)
          `(setf (gethash ,(car pair) ,retn) ,(cadr pair)))
         pairs)
      ,retn)))))

(set-pprint-dispatch 'hash-table
 (lambda (str ht)
  (format str "{~{~{~S => ~S~}~^, ~}}"
   (loop for key being the hash-keys of ht
         for value being the hash-values of ht
      collect (list key value)))))

;create instance struct
(defmacro new-struct
	 (type slots-and-values)
       (let ((ptr (gensym)))
	   `(let* ((,ptr (cffi:foreign-alloc '(:struct ,type))))
	      ,(loop for (slot value) in slots-and-values
		  collect `(cffi:foreign-slot-value ,ptr '(:struct ,type) ',slot)
		  into foreign-slot-values
		  collect value
		  into values
		  finally (return (append '(setf) (interleave foreign-slot-values values))))
	      ,ptr)))





;NOTHING GOES PAST THIS.
(export-all-symbols-except nil)
  
