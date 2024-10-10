(in-package #:quaviver)

(defmacro %float-bits (float-type value)
  (float-bits-form float-type value))

#+quaviver/short-float
(defmethod float-bits ((float-type (eql 'short-float)) value)
  (%float-bits short-float (coerce value 'short-float)))

#-quaviver/short-float
(defmethod float-bits ((float-type (eql 'short-float)) value)
  (%float-bits single-float (coerce value 'single-float)))

(defmethod float-bits ((float-type (eql 'single-float)) value)
  (%float-bits single-float (coerce value 'single-float)))

(defmethod float-bits ((float-type (eql 'double-float)) value)
  (%float-bits double-float (coerce value 'double-float)))

#+quaviver/long-float
(defmethod float-bits ((float-type (eql 'long-float)) value)
  (%float-bits long-float (coerce value 'long-float)))

#-quaviver/long-float
(defmethod float-bits ((float-type (eql 'long-float)) value)
  (%float-bits double-float (coerce value 'double-float)))

