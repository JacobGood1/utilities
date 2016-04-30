(in-package :math)

(def-class point
    :slots ((px 0) (py 0)))

(defmethod initialize-instance :after
    ((point point) &key px py)
  (setf (:px point) px
	(:py point) py))

(defmethod scale
    ((point point) value)
  (setf (:px point) (* value (:px point))
	(:py point) (* value (:py point)))
  point)
