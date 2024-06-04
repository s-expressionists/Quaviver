(cl:in-package #:quaviver)

(defgeneric bits-to-float (client result-type bits))

(defgeneric float-to-bits (client value))

(defgeneric digits-to-float (client result-type digits exponent))

(defgeneric float-to-digits (client value))
