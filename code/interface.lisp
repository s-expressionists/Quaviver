(cl:in-package #:quaviver)

(defgeneric bits-float (client result-type bits))

(defgeneric float-bits (client value))

(defgeneric integer-float (client result-type base significand exponent sign))

(defgeneric float-integer (client base value))

(defgeneric digits-integer (client base digits part &optional start end limit))

(defgeneric integer-digits (client result-type base value part))

(defgeneric digit-integer (client base value part count))

(defgeneric integer-digit (client base value part count))

(defgeneric sequence-float (client result-type base sequence &optional start end))

(defgeneric float-sequence (client result-type base value))

(defgeneric storage-size (type))

(defgeneric significand-bytespec (type))

(defgeneric exponent-bytespec (type))

(defgeneric sign-bytespec (type))

(defgeneric nan-payload-bytespec (type))

(defgeneric nan-type-bytespec (type))

(defgeneric hidden-bit-p (type))

(defgeneric exponent-bias (type))

(defgeneric max-exponent (type))

(defgeneric min-exponent (type))

(defgeneric significand-size (type)
  (:method (type)
    (if (hidden-bit-p type)
        (1+ (byte-size (significand-bytespec type)))
        (byte-size (significand-bytespec type)))))

(defgeneric arithmetic-size (type))
