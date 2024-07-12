(in-package #:quaviver)

#+quaviver/short-float
(defmethod integer-float
    (client (float-type (eql 'short-float)) (base (eql 2)) significand exponent sign)
  (declare (ignore client))
  (internal-integer-float/short-float significand exponent sign))

(defmethod integer-float
    (client (float-type (eql 'single-float)) (base (eql 2)) significand exponent sign)
  (declare (ignore client))
  (internal-integer-float/single-float significand exponent sign))

(defmethod integer-float
    (client (float-type (eql 'double-float)) (base (eql 2)) significand exponent sign)
  (declare (ignore client))
  (internal-integer-float/double-float significand exponent sign))

#+quaviver/long-float
(defmethod integer-float
    (client (float-type (eql 'long-float)) (base (eql 2)) significand exponent sign)
  (declare (ignore client))
  (internal-integer-float/long-float significand exponent sign))
