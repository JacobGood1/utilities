(in-package :math)

;;TODO change vector to store a Point
(def-class vector :slots ((vx 0) (vy 0)))

(defmethod initialize-instance :after
    ((vector vector) &key vx vy)
  (setf (:vx vector) vx
	(:vy vector) vy))

;;(defun get-magnitude
;;    (vector)
;;  (let* ((x (:vx vector))
;;	 (y (:vy vector)))
;;    (sqrt (+ (* x x) (* y y)))))

;;(defun normalize-vector
;;    (vector)
;;  (let* ((x (:vx vector))
;;	 (y (:vy vector))
;;	 (m (get-magnitude vector)))
;;    (if (> m 0)
;;	(list (/ x m) (/ y m))
;;	(progn (print "Can't normalize vector with magnitude of 0...")))))

(defmethod get-magnitude
    ((vector vector))
  (let* ((x (:vx vector))
	 (y (:vy vector)))
    (sqrt (+ (* x x) (* y y)))))

(defmethod normalize-vector
    ((vector vector))
  (let* ((x (:vx vector))
	 (y (:vy vector))
	 (m (get-magnitude vector)))
    (if (> m 0)
	(progn
	  (setf (:vx vector) (/ x m)
		(:vy vector) (/ y m))
	  vector)
	(progn (print "Can't normalize vector with magnitude of 0...")))))

(defmethod scale-vector
    ((vector vector) value)
  (let* ((x (:vx vector))
	 (y (:vy vector)))
    (progn
      (setf (:vx vector) (* value x)
	    (:vy vector) (* value y)))))
