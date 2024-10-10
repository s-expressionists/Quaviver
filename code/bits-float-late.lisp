(in-package #:quaviver)

(defmacro %external-float (float-type value)
  (let ((implementation-type (implementation-type float-type)))
    (if (exact-implementation-type-p float-type)
        `(%bits-float ,implementation-type ,value)
        `(multiple-value-bind (significand exponent sign)
             ,(bits-primitive-triple-form float-type value)
           ,(primitive-triple-float-form implementation-type 'significand 'exponent 'sign)))))

(defmethod bits-float ((float-type (eql :bfloat16)) value)
  (%external-float :bfloat16 value))

(defmethod bits-float ((float-type (eql :binary16)) value)
  (%external-float :binary16 value))

(defmethod bits-float ((float-type (eql :binary32)) value)
  (%external-float :binary32 value))

(defmethod bits-float ((float-type (eql :binary64)) value)
  (%external-float :binary64 value))

(defmethod bits-float ((float-type (eql :binary80)) value)
  (%external-float :binary80 value))

(defmethod bits-float ((float-type (eql :binary128)) value)
  (%external-float :binary128 value))

(defmethod bits-float ((float-type (eql :binary256)) value)
  (%external-float :binary256 value))
