(cl:in-package #:quaviver)

(defgeneric bits-to-float (client bits))

(defgeneric float-to-bits (client value))

(defgeneric digits-to-float (client digits exponent))

(defgeneric float-to-digits (client value))
