(cl:in-package #:quaviver)

(defgeneric bits-float (client result-type bits))

(defgeneric float-bits (client value))

(defgeneric integer-float (client result-type base significand exponent sign))

(defgeneric float-integer (client base value))

(defgeneric digits-integer (client base digits))

(defgeneric integer-digits (client result-type base value))

(defgeneric storage-size (type))

(defgeneric significand-bytespec (type))

(defgeneric significand-byte-form (type))

(defgeneric significand-size (type)
  (:method (type)
    (if (hidden-bit-p type)
        (1+ (byte-size (significand-bytespec type)))
        (byte-size (significand-bytespec type)))))

(defgeneric exponent-bytespec (type))

(defgeneric exponent-byte-form (type))

(defgeneric exponent-size (type)
  (:method (type)
    (byte-size (exponent-bytespec type))))

(defgeneric sign-bytespec (type))

(defgeneric sign-byte-form (type))

(defgeneric sign-size (type)
  (:method (type)
    (byte-size (sign-bytespec type))))

(defgeneric nan-payload-bytespec (type))

(defgeneric nan-payload-byte-form (type))

(defgeneric nan-type-bytespec (type))

(defgeneric nan-type-byte-form (type))

(defgeneric hidden-bit-p (type))

(defgeneric exponent-bias (type)
  (:method (type)
    (+ (ash 1 (1- (exponent-size type)))
       (significand-size type)
       -2)))

(defgeneric max-exponent (type)
  (:method (type)
    (- (ash 1 (exponent-size type))
       (exponent-bias type)
       2)))

(defgeneric min-exponent (type)
  (:method (type)
    (- 2
       (exponent-bias type)
       (significand-size type))))

(defgeneric arithmetic-size (type))
