(in-package #:quaviver)

(defmacro %bits-float (float-type value)
  (bits-float-form float-type value))

#+quaviver/short-float
(defmethod bits-float ((float-type (eql 'short-float)) value)
  (%bits-float short-float value))

(defmethod bits-float ((float-type (eql 'single-float)) value)
  (%bits-float single-float value))

(defmethod bits-float ((float-type (eql 'double-float)) value)
  (%bits-float double-float value))

#+quaviver/long-float
(defmethod bits-float ((float-type (eql 'long-float)) value)
  (%bits-float long-float value))
