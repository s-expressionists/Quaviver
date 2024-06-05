(cl:in-package #:quaviver)

(defgeneric bits-float (client result-type bits))

(defgeneric float-bits (client value))

(defgeneric digits-float (client result-type digits exponent sign))

(defgeneric float-digits (client value))
