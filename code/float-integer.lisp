(in-package #:quaviver)

#+clisp
(defmethod float-integer (client (base (eql 2)) value)
  (declare (ignore client))
  (etypecase value
    #+quaviver/short-float
    (short-float
     (float-internal-integer/short-float value))
    (single-float
     (float-internal-integer/single-float value))
    (double-float
     (float-internal-integer/double-float value))
    #+quaviver/long-float
    (long-float
     (float-internal-integer/long-float value))))

#+(and (not clisp) quaviver/short-float)
(defmethod float-integer (client (base (eql 2)) (value short-float))
  (declare (ignore client))
  (float-internal-integer/short-float value))

#-clisp
(defmethod float-integer (client (base (eql 2)) (value single-float))
  (declare (ignore client))
  (float-internal-integer/single-float value))

#-clisp
(defmethod float-integer (client (base (eql 2)) (value double-float))
  (declare (ignore client))
  (float-internal-integer/double-float value))

#+(and (not clisp) quaviver/long-float)
(defmethod float-integer (client (base (eql 2)) (value long-float))
  (declare (ignore client))
  (float-internal-integer/long-float value))
