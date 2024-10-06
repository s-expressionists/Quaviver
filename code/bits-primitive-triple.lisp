(in-package #:quaviver)

(defmacro %bits-primitive-triple (float-type bits-var)
  (bits-primitive-triple-form float-type bits-var))

(defmethod bits-primitive-triple ((float-type (eql :bfloat16)) value)
  (%bits-primitive-triple :bfloat16 value))

(defmethod bits-primitive-triple ((float-type (eql :binary16)) value)
  (%bits-primitive-triple :binary16 value))

(defmethod bits-primitive-triple ((float-type (eql :binary32)) value)
  (%bits-primitive-triple :binary32 value))

(defmethod bits-primitive-triple ((float-type (eql :binary64)) value)
  (%bits-primitive-triple :binary64 value))

(defmethod bits-primitive-triple ((float-type (eql :binary80)) value)
  (%bits-primitive-triple :binary80 value))

(defmethod bits-primitive-triple ((float-type (eql :binary128)) value)
  (%bits-primitive-triple :binary128 value))

(defmethod bits-primitive-triple ((float-type (eql :binary256)) value)
  (%bits-primitive-triple :binary256 value))
