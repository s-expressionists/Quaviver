(in-package #:quaviver/native)

(defmethod quaviver:integer-float ((client client) result-type base significand exponent sign)
  (coerce (if (minusp exponent)
              (/ significand (expt base (- exponent)))
              (* significand (expt base exponent)))
          result-type))
