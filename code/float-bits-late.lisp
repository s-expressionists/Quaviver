(in-package #:quaviver)

(defmacro %float-external (float-type value)
  (let ((implementation-type (implementation-type float-type)))
    (if (exact-implementation-type-p float-type)
        `(%float-bits ,implementation-type (coerce ,value ',implementation-type))
        `(multiple-value-bind (significand exponent sign)
             ,(float-primitive-triple-form implementation-type `(coerce ,value ',implementation-type))
           ,(primitive-triple-bits-form float-type 'significand 'exponent 'sign)))))

(defmethod float-bits ((float-type (eql :bfloat16)) value)
  (%float-external :bfloat16 value))

(defmethod float-bits ((float-type (eql :binary16)) value)
  (%float-external :binary16 value))

(defmethod float-bits ((float-type (eql :binary32)) value)
  (%float-external :binary32 value))

(defmethod float-bits ((float-type (eql :binary64)) value)
  (%float-external :binary64 value))

(defmethod float-bits ((float-type (eql :binary80)) value)
  (%float-external :binary80 value))

(defmethod float-bits ((float-type (eql :binary128)) value)
  (%float-external :binary128 value))

(defmethod float-bits ((float-type (eql :binary256)) value)
  (%float-external :binary256 value))
