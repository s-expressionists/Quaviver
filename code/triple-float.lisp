(in-package #:quaviver)

#+quaviver/short-float
(defmethod triple-float
    (client (float-type (eql 'short-float)) (base (eql 2)) significand exponent sign)
  (declare (ignore client))
  (primitive-triple-float/short-float significand exponent sign))

(defmethod triple-float
    (client (float-type (eql 'single-float)) (base (eql 2)) significand exponent sign)
  (declare (ignore client))
  (primitive-triple-float/single-float significand exponent sign))

(defmethod triple-float
    (client (float-type (eql 'double-float)) (base (eql 2)) significand exponent sign)
  (declare (ignore client))
  (primitive-triple-float/double-float significand exponent sign))

#+quaviver/long-float
(defmethod triple-float
    (client (float-type (eql 'long-float)) (base (eql 2)) significand exponent sign)
  (declare (ignore client))
  (primitive-triple-float/long-float significand exponent sign))

#-quaviver/short-float
(defmethod triple-float
    (client (float-type (eql 'short-float)) base significand exponent sign)
  (triple-float client 'single-float base significand exponent sign))

#-quaviver/long-float
(defmethod triple-float
    (client (float-type (eql 'long-float)) base significand exponent sign)
  (triple-float client 'double-float base significand exponent sign))
