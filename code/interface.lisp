(cl:in-package #:quaviver)

(defgeneric bits-float (client result-type bits))

(defgeneric float-bits (client value))

(defgeneric integer-float (client result-type base digits exponent sign))

(defgeneric float-integer (client base value))

(defgeneric digits-integer (client base digits))

(defgeneric integer-digits (client result-type base value))
