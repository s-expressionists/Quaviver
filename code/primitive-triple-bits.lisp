(in-package #:quaviver)

(defmacro %primitive-triple-bits (float-type significand exponent sign)
  (primitive-triple-bits-form float-type significand exponent sign))

(defmethod primitive-triple-bits ((float-type (eql :bfloat16)) significand exponent sign)
  (%primitive-triple-bits :bfloat16 significand exponent sign))

(defmethod primitive-triple-bits ((float-type (eql :binary16)) significand exponent sign)
  (%primitive-triple-bits :binary16 significand exponent sign))

(defmethod primitive-triple-bits ((float-type (eql :binary32)) significand exponent sign)
  (%primitive-triple-bits :binary32 significand exponent sign))

(defmethod primitive-triple-bits ((float-type (eql :binary64)) significand exponent sign)
  (%primitive-triple-bits :binary64 significand exponent sign))

(defmethod primitive-triple-bits ((float-type (eql :binary80)) significand exponent sign)
  (%primitive-triple-bits :binary80 significand exponent sign))

(defmethod primitive-triple-bits ((float-type (eql :binary128)) significand exponent sign)
  (%primitive-triple-bits :binary128 significand exponent sign))

(defmethod primitive-triple-bits ((float-type (eql :binary256)) significand exponent sign)
  (%primitive-triple-bits :binary256 significand exponent sign))

