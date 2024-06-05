(cl:in-package #:quaviver)

(defgeneric bits-float (client result-type bits))

(defgeneric float-bits (client value))

(defgeneric decimal-float (client result-type digits exponent sign))

(defgeneric float-decimal (client value))
