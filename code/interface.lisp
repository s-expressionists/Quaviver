(cl:in-package #:quaviver)

(defgeneric make-client (name &rest initargs &key))

(defgeneric float-to-digits (client value))

(defgeneric digits-to-float (client digits exponent))
