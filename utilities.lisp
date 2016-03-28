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

(defmacro ignore-warnings
    (&rest code)
  `(locally
       (declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))
     (declare #+sbcl(sb-ext:muffle-conditions style-warning))
    (handler-bind
	 (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))
       ,@code)))

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
    (name class args &rest body)
  (if (keywordp name)
      (let* ((args (setf args (append args (list (list class class))))))
	`(defmethod ,name 
	     ,args 
	   (with-accessors ,(loop for x in (slots-of class) collect (list x (to-keyword x)))
	       ,class
	     ,@body)))
      (error "Name of the object MUST be a keyword!")))

(def-generic fetch (coll item-to-fetch))

;  (def-method fetch ((coll hash-table) k)
;    (gethash k coll))

 ;;;!!! code for dependency initialization, conform it to the new system!

;;temporarily creating a new implimentation
;;TODO remove inlining, make raw slots instead

;(let ((raw-class-private (all-permutations '(:raw :private :class)))
;      (raw-class-nil (all-permutations '(:raw :class nil)))
;      (raw-private-nil (all-permutations '(:raw :private nil)))
;      (private-class-nil (all-permutations '(:private :class nil)))
;      (raw-nil-nil '((:raw nil nil) (nil :raw nil) (nil nil :raw)))
;      (private-nil-nil '((:private nil nil) (nil :private nil) (nil nil :private)))
;      (class-nil-nil '((:class nil nil) (nil :class nil) (nil nil :class)))
;      (nil-nil-nil '(nil nil nil)))
;  (interpose raw-class-private '(setf :initarg)))

(defparameter *class-slots* (make-hash-table))
(defparameter *super-args* (make-hash-table))
(defparameter *hierarchy* (make-hash-table))

(defmacro def-class
<<<<<<< HEAD
    (name &key extends slots constructor dependencies)
  
  (eval `(defclass ,name ,extends ,(loop for (slot value opt1 opt2) in slots collect slot)))
  
  (closer-mop:finalize-inheritance (find-class name))
=======
    (name &key extends slots constructor super-args dependencies)
>>>>>>> parent of f524d57... can call supers now

  (multiple-value-bind
	(slots-for-class slots-for-class-slots)
	(loop for (slot value opt1 opt2) in slots
	   collect slot into slots
	   finally (return (values slots slots)))
    (eval `(defclass ,name ,extends ,slots-for-class))
    (setf (gethash name *class-slots*) (remove-duplicates (append slots-for-class-slots (flatten (loop for o in extends collect (slots-of o)))))))

  (if (and (nil? slots)
	   (nil? extends))
      (warn (to-string name " has no slots...")))

  (if constructor
      (progn (if (eq (first constructor) 'lambda)
		 nil
		 (error "constructor must be a lambda expression"))
	     (setf (gethash name *super-args*) (second constructor))))

  (if dependencies
      (setf (gethash name *class-dependencies*) dependencies))
  
  (if extends
      (setf (gethash name *hierarchy*) extends))
  
  (loop
     with objs-with-deps = (loop
			      for obj in extends
			      collect (if (gethash obj *class-dependencies*)
					  (list obj (gethash obj *class-dependencies*))))
       
     with objs-to-check = (append extends (list name))
       
     for (object dependencies) in objs-with-deps
     do (loop
	   for dep in dependencies
	   do (if (not (in? objs-to-check dep))
		  (error (to-string "The class, "
				    object
				    ", depends upon, "
				    dep
				    ", which is not this class or any class that "
				    name
				    " extends!")))))
  
  (setf extends (append extends '(printer-base)))
 
  (let* ((slots-and-values slots)
	 (constructor-code nil)
	 (hierarchy (find-hierarchy name))
	 (all-slots (-> (loop
			   for class in hierarchy
			   collect (slots-of class))
		      (append slots)
		      flatten))
	 (constructor-args (append (-> (loop
					  for obj in hierarchy
					  when (not (eq obj 'printer-base))
					  collect (loop for slot in (gethash obj *super-args*)
						     collect slot))
				     flatten)
				   (first (rest constructor))))
	 (constructor-requirements constructor-args)
	 (constructor-body (cddr constructor)))

<<<<<<< HEAD
    ;warn user when the constructor arguements from various classes are shadowing each other
    (loop
       with inner-counter     = 0
       with outer-counter     = 0
       with current-arg       = nil
       with current-inner-arg = nil
       with already-warned    = '()
       while (< outer-counter (length constructor-args))
       do (progn
	    (setf current-arg (elt constructor-args outer-counter))
	    (loop
	       while (< inner-counter (length constructor-args))
	       do (progn
		    (setf current-inner-arg (elt constructor-args inner-counter))
		    (if (and (eq current-arg current-inner-arg)
			     (not (= inner-counter outer-counter)))
			(if-not (in? already-warned current-inner-arg)
				(progn (warn (to-string "You are shadowing constructor arguement: " current-inner-arg))
				       (setf already-warned (append already-warned (list current-inner-arg))))))
		    (setf inner-counter (1+ inner-counter))))
	    (setf outer-counter (1+ outer-counter))
	    (setf inner-counter 0)))

    ;warn user when slots from various classes are shadowing each other
    (loop
       with inner-counter     = 0
       with outer-counter     = 0
       with current-arg       = nil
       with current-inner-arg = nil
       with already-warned    = '()
       while (< outer-counter (length all-slots))
       do (progn
	    (setf current-arg (elt all-slots outer-counter))
	    (loop
	       while (< inner-counter (length all-slots))
	       do (progn
		    (setf current-inner-arg (elt all-slots inner-counter))
		    (if (and (eq current-arg current-inner-arg)
			     (not (= inner-counter outer-counter)))
			(if-not (in? already-warned current-inner-arg)
				(progn (warn (to-string "You are shadowing slot "
							current-inner-arg
							" with another slot "
							current-inner-arg))
				       (setf already-warned (append already-warned (list current-inner-arg))))))
		    (setf inner-counter (1+ inner-counter))))
	    (setf outer-counter (1+ outer-counter))
	    (setf inner-counter 0)))
    
    ;get rid of duplicates in the constructor args, all of the supers wil share the same arguement
    (setf constructor-args (remove-duplicates constructor-args))
   
    (setf constructor-code 
	  `(defmethod initialize-instance
	       :after
	     ((,name ,name) &key ,@constructor-args) 
	     ,@constructor-body
	     (when (next-method-p)
	       `(call-next-method))))
=======
    (setf constructor-code (if constructor
			       `(defmethod initialize-instance
				    :after
				  ((,name ,name) &key ,@constructor-args) 
				  ,@constructor-body
				  (when (next-method-p)
				    ,(if super-args `(call-next-method ,name ,@super-args) `(call-next-method ,name))))))
>>>>>>> parent of f524d57... can call supers now
    
   ;TODO this needs fixing, patterns are a fail for now
    (let* ((class nil)
	   (export-list '()))
      (multiple-value-bind (slots-and-values setup-interface getters setters)
	  (loop for (slot value opt1 opt2) in slots-and-values
	       for slot-key = (to-keyword slot)
	     collect (trivia:match `(,opt1 ,opt2)
		       ('(:raw nil)       (setf export-list (append export-list (list slot)))
			                  `(,slot :initarg ,(to-keyword slot) :initform ,value))
		       ('(nil :raw)       (setf export-list (append export-list (list slot)))
			                  `(,slot :initarg ,(to-keyword slot) :initform ,value))
		       ('(:raw :private) `(,slot :initarg ,(to-keyword slot) :initform ,value))
		       ('(:private :raw) `(,slot :initarg ,(to-keyword slot) :initform ,value))
		       ('(:private nil)  `(,slot :initarg ,(to-keyword slot) :initform ,value :accessor ,slot))
		       ('(nil :private)  `(,slot :initarg ,(to-keyword slot) :initform ,value :accessor ,slot))
		       ('(nil nil)        (setf export-list (append export-list (list slot)))
			
			                 `(,slot :initarg ,(to-keyword slot) :initform ,value :accessor ,slot-key))
		       (otherwise (error
				   (to-string "Error! Slot options are incorrect... 
                                               expected option of :raw, :private, not kek"))))
	     into slots-and-values
	     collect (if (not (gethash slot-key *keys-that-exist-already*))
			 `(progn
			    ,(setf (gethash slot-key *keys-that-exist-already*) t)
			    (declaim (inline ,slot-key))
			    (defgeneric (setf ,slot-key)
				(value object-map-or-vector)
			      (:generic-function-class inlined-generic-function:inlined-generic-function))
			    (defgeneric ,slot-key
				(object-map-or-vector)
			      (:generic-function-class inlined-generic-function:inlined-generic-function))))
	     into setup-interface
	     collect `(defmethod ,slot-key ((,name ,name)) (slot-value ,name ',slot))
	     into getters
	     collect `(defmethod (setf ,slot-key) (value (,name ,name)) (setf (slot-value ,name ',slot) value))
	     into setters
	     finally (return (values slots-and-values setup-interface getters setters)))

	
	(setf slots-and-values (append slots-and-values `((name :initform ',name :reader name :allocation :class))))
	
	(setf class `(defclass ,name ,extends ,slots-and-values))
	
<<<<<<< HEAD
	`(ignore-warnings
	  (progn
	    ,class
					;,@setup-interface
					;,@getters
					;,@setters
	    ,constructor-code
	    
	    
	    (defmacro ,name
		(&key ,@constructor-args)
	      
	      (if ',dependencies
		  (error "This class has dependencies, it cannot be instantiated directly"))
	      
	      (if ',constructor-requirements
		  (if-not (and ,@constructor-requirements)
			  (error (format nil "Please provide the following arguements for the constructor: ~A"
					 (loop
					    for arg in ',constructor-requirements
					    collect (loop
						       for a in ',constructor-args
						       when (eq a arg)
						       return arg))))))
	      `(make-instance ',',name
			      ,@(loop
				   with code = '()
				   for elm in
				     (loop
					for (arg value) in (partition
							    (interleave ',constructor-args
									(list ,@constructor-args))
							    2)
					when (not (nil? value)) 
					collect (list (to-keyword arg) value))
				   do (setf code (append code elm))
				   finally (return code))))
=======
	`(progn
	   
	   ,class
	   ;,@setup-interface
	   ;,@getters
	   ;,@setters
	   ,constructor-code
	   

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
			     ,@(loop
				  with code = '()
				  for elm in
				    (loop
				       for (arg value) in (partition
							   (interleave ',constructor-args
								       (list ,@constructor-args))
							   2)
				       when (not (nil? value)) 
				       collect (list (to-keyword arg) value))
				  do (setf code (append code elm))
				  finally (return code))))
>>>>>>> parent of f524d57... can call supers now
					;deleted export list check to see if still gone
	    (export ',name)
	    "SUCCESS"))))))




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
  (multiple-value-bind (defun-args types-code)
      (generate-type-information args)
    `(progn (declaim (inline ,name))
	    (defun ,name
		,defun-args
	      (declare (optimize (speed 3) (safety 0)))
	      ,@types-code
	      ,@code))))

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




;defun redefinition exports symbol automatically
(defmacro defn (name args &rest body)
  `(progn (defun ,name ,args ,@body)
	  (export ',name)))

;private version
(defmacro defn- (name args &rest body)
  `(defun ,name ,args ,@body))

;defmacro redefinition exports symbol automatically
(defmacro def-spel (name args &rest body)
  `(progn (defmacro ,name ,args ,@body)
	  (export ',name)))
;private version
(defmacro def-spel- (name args &rest body)
  `(defmacro ,name ,args ,@body))



(defmacro make-map (&rest args)
  (let* ((map (make-hash-table))
	 (args (loop 
		  for (key val) in (partition args 2)
		  collect (list key (eval val))))
	 (generate-keyword-functions
	       (loop
		  for (key val) in args
		  do (setf (gethash key map) val)
		  collect (if (keywordp key)
			    `(defmethod ,key ((hash hash-table)) (gethash ,key hash))))))
    (locally
	(declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))
      `(handler-bind
	   (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))
	 (progn ,@generate-keyword-functions
		,map)))))

;;
;NOTHING GOES PAST THIS.
(export-all-symbols-except nil)
  
