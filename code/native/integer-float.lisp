(in-package #:quaviver/native)

(defmethod quaviver:integer-float ((client client) result-type base significand exponent sign)
  (let ((significand (if (minusp sign) (- significand) significand)))
    (coerce (if (minusp exponent)
                (/ significand (expt base (- exponent)))
                (* significand (expt base exponent)))
            result-type)))

(defmethod quaviver:integer-float
    ((client client) (result-type (eql 'single-float)) base (significand (eql 0)) exponent sign)
  (if (minusp sign) -0f0 0f0))

(defmethod quaviver:integer-float
    ((client client) (result-type (eql 'double-float)) base (significand (eql 0)) exponent sign)
  (if (minusp sign) -0d0 0d0))

#+quaviver/long-float
(defmethod quaviver:integer-float
    ((client client) (result-type (eql 'long-float)) base (significand (eql 0)) exponent sign)
  (if (minusp sign) -0l0 0l0))
