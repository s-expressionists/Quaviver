(in-package #:quaviver/native)

(defmethod quaviver:triple-float ((client client) float-type base significand exponent sign)
  (let ((significand (if (minusp sign) (- significand) significand)))
    (coerce (if (minusp exponent)
                (/ significand (expt base (- exponent)))
                (* significand (expt base exponent)))
            float-type)))

(defmethod quaviver:triple-float
    ((client client) (float-type (eql 'single-float)) base (significand (eql 0)) exponent sign)
  (declare (ignore base exponent))
  (if (minusp sign) -0f0 0f0))

(defmethod quaviver:triple-float
    ((client client) (float-type (eql 'double-float)) base (significand (eql 0)) exponent sign)
  (declare (ignore base exponent))
  (if (minusp sign) -0d0 0d0))

#+quaviver/long-float
(defmethod quaviver:triple-float
    ((client client) (float-type (eql 'long-float)) base (significand (eql 0)) exponent sign)
  (declare (ignore base exponent))
  (if (minusp sign) -0l0 0l0))
