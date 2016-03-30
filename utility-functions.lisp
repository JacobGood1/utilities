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

;TODO make this preserve the case one day
;WARNING make sure everything in code is lower case
(defun to-string (&rest args)
    (apply #'concatenate 
	   'string 
	   (loop for i in args 
	      collect  (if (stringp i) 
			   i 
			   (write-to-string i :case :downcase)))))
  
  (defun to-symbol (str) (intern (string-upcase str)))

  (defun to-keyword (name) 
    (values (intern (string-upcase (if (or (typep name 'integer)
					   (typep name 'float))
				       (write-to-string name)
				       name))
				       "KEYWORD")))
  
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
  
(defun find-hierarchy
    (class)
  (flatten (loop for c in (gethash class *hierarchy*)
	      collect (list c (find-hierarchy c)))))

 ;TODO need to make first work on every seqable structure!
(defgeneric 1st (seq))

  (defmethod 1st ((str string))
    (char str 0))

  (defmethod 1st ((lst cons))
    (funcall lisp-first lst))

  (defmethod 1st ((coll null))
    (funcall lisp-first coll))

  (defmethod 1st ((coll vector))
    (if (not (= (length coll)
		0))
	(aref coll 0)
	nil))

(defparameter *class-dependencies* (make-hash-table))
(defparameter *keys-that-exist-already* (make-hash-table))
;TODO test to see if slots of is working right with methods and inheritance

;(defun slots-of
;    (class)
;  (gethash class *class-slots*))

(defun slots-of
    (class)
  (if (not (nil? class))
      (let ((deps (append (gethash class *class-dependencies*) (list class))))
	(remove-duplicates (flatten (loop for class in deps
				       collect (loop for slot in (closer-mop:class-slots (find-class class))
						  collect (closer-mop:slot-definition-name slot))))))))


      
	
	
	

(defun finalize-class
    (class)
  (if (not (in? '(integer string float symbol) class)) 
      (closer-mop:finalize-inheritance (find-class class))))

(defun gen-slots (slots)
  (loop for (a b) in slots
     collect `(,a
	       :initarg  ,(to-keyword a) 
	       :initform ,b 
	       :accessor ,a)))

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
    ;;;IF NOT DOES NOT EXIST YET!!!11
  `(with-accessors ,(loop for slot in (slots-of (second (first obj-list)))
		       collect (list slot slot)) 
       ,(first (first obj-list))))

(defun make-slot-args
    (obj-list)
  "takes an argument list that would normally
   go to the defmethod function...
   '((point1 one) (point2 two))
   it then obtains the slots of the objects that defmethod dispatches on
   assuming one and two are points...
   ((x y) (x y))
   it then takes these slots and matches them with their object like so...
   ((WITH-slots ((POINT1-X X) (POINT1-Y Y)) ONE)
    (WITH-slots ((POINT2-X X) (POINT2-Y Y)) TWO) NIL)"
    ;;;IF NOT DOES NOT EXIST YET!!!11
  (loop for (var obj) in obj-list
     for slots = (slots-of obj)
     if slots
     collect `(with-slots ,(loop for slot in slots
			      collect (list (to-symbol (to-string var "-" slot)) 
					    slot)) 
		  ,var)))

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


(defun generate-with-slots
    (objects code)
  (arrow-macros:-> (make-slot-args objects)
    (place-method-at-the-end-of-with-accessors code)
    nest-with-accessors))

(defun generate-with-accessors
    (objects code)
  "code should be a list of lists!"
  (arrow-macros:-> (make-accessor-args objects)
    (append code)))

(defun generate-type-information
    (types)
  "types = ((x integer) (y something)) the function must take a list of lists designating their type"
  (loop for (arg type) in types
     collect arg into args
     collect (if type `(declare (type ,type ,arg))) into types-code
     finally (return (values args types-code))))


(defun combine-slots (classes)
    (remove-duplicates 
     (flatten 
      (loop for class in classes collect (slots-of class)))))

  (defun grab-slots (slots)
    (loop for (a b) in (partition slots 2) collect a))

  (defun grab-slots-with-accessors (slots)
    (loop for slot in slots collect (list slot slot)))

;;
;printer-base is an object that ecery object derives from
  ;it will ensure that all object created by def-class are printed much readable
(defclass printer-base () ())

(defun print-it
    (object indentation-level)
  (loop
     with obj-string = (to-string "object! " (name object) " [~%")
       with args = '()
     for slot in (slots-of (name object))
       
     when (not (eq slot 'name))  
     do (let* ((value (funcall (to-keyword slot) object)))
	  
	  (if (and (not (in? '(single-float ratio integer fixnum t null boolean bit simple-vector vector hash-table)
			     (class-name (class-of value))))
		   (not (eq value t)))
	      (progn
		
		(setf obj-string (concatenate 'string obj-string
					      (to-string "~"
							 (+ 4 indentation-level)
							 "@a: "
							 (print-it value
								   (+ 4 indentation-level)))))
		(setf args (append args (list slot (multiple-value-bind (_ args) (print-it value indentation-level) args)))))
	      (progn
		(setf obj-string (concatenate 'string obj-string (to-string "~" (+ 4 indentation-level) "@a: ~a~%")))
		(setf args (append args (list slot (if (nil? value) 'null value)))))))
     finally (return (values (to-string obj-string (format nil (to-string "~" indentation-level "a") "") "]~%")
			     args)))) 


;print-object makes all objects of the type printer-base print more readable
(defmethod print-object ((object printer-base) stream)
  (apply #'format stream (multiple-value-bind (string args) (print-it object 0) `(,string ,@(flatten args)))))

(defgeneric attach (coll &rest vals))

(defmethod attach ((coll hash-table) &rest vals)
  (loop
     for (key value) in (partition vals 2)
     do (if (nil? value)
	    (error "A value and key must be supplied to the method attach when invoked on a map")
	    (setf (gethash key coll) value)))
  coll) 
  
(defmethod attach ((coll vector) &rest vals)
  (loop for value in vals
     do (vector-push-extend value coll))
  coll)

(defmethod attach ((coll cons) &rest vals)
  (loop
     for val in vals
     do (setf coll (cons val coll)))
  coll)

(declaim (inline list?))
(defun list?
    (list?)
  (consp list?))

;;;into
(defun into-internal
    (obj list)
  (loop
     with obj = (funcall (first list) obj)
     for s in (rest list)
     do (setf obj (funcall s obj))
     finally (return obj)))

(defmacro into
    (obj &rest fn-slots)
  `(into-internal ,obj ',fn-slots))


(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
	      append (mapcar (lambda (l) (cons element l))
			     (all-permutations (remove element list)))))))

(defun interpose
    (list value)
  (loop with ret = '()
     for x in list
     do (setf ret (append ret (list x value)))
     finally (return ret)))
