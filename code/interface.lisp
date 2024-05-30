(cl:in-package #:quaviver)

(defgeneric float-to-digits (client value))

(defgeneric digits-to-float (client digits exponent))
