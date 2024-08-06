(in-package #:quaviver)

#+clisp
(defmethod float-triple (client (base (eql 2)) value)
  (declare (ignore client))
  (etypecase value
    #+quaviver/short-float
    (short-float
     (float-primitive-triple/short-float value))
    (single-float
     (float-primitive-triple/single-float value))
    (double-float
     (float-primitive-triple/double-float value))
    #+quaviver/long-float
    (long-float
     (float-primitive-triple/long-float value))))

#+(and (not clisp) quaviver/short-float)
(defmethod float-triple (client (base (eql 2)) (value short-float))
  (declare (ignore client))
  (float-primitive-triple/short-float value))

#-clisp
(defmethod float-triple (client (base (eql 2)) (value single-float))
  (declare (ignore client))
  (float-primitive-triple/single-float value))

#-clisp
(defmethod float-triple (client (base (eql 2)) (value double-float))
  (declare (ignore client))
  (float-primitive-triple/double-float value))

#+(and (not clisp) quaviver/long-float)
(defmethod float-triple (client (base (eql 2)) (value long-float))
  (declare (ignore client))
  (float-primitive-triple/long-float value))
