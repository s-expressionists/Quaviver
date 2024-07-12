(cl:in-package #:quaviver)

(defgeneric bits-float (float-type bits))

(defgeneric bits-float-form (float-type bits))

(defgeneric float-bits (value))

(defgeneric float-bits-form (type value))

(defgeneric integer-float (client float-type base significand exponent sign))

(defgeneric internal-integer-float-form (float-type significand exponent sign))

(defgeneric float-integer (client base value))

(defgeneric float-internal-integer-form (float-type value))

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

(defgeneric subnormalp (type))

(defgeneric non-number-p (type))

(defgeneric internal-base (type))

(defgeneric exponent-bias (type)
  (:method (type)
    (+ (ash 1 (1- (exponent-size type)))
       (significand-size type)
       (if (non-number-p type) -2 -1))))


(defgeneric max-exponent (type)
  (:method (type)
    (- (ash 1 (exponent-size type))
       (exponent-bias type)
       2)))

(defgeneric min-exponent (type)
  (:method (type)
    (if (or (subnormalp type)
            (not (non-number-p type)))
        (- 2
           (exponent-bias type)
           (significand-size type))
        (- 1
           (exponent-bias type)))))

(defgeneric arithmetic-size (type))

(deftype significand-word (type &optional (extra 0))
  `(unsigned-byte ,(+ (significand-size type) extra)))

(deftype exponent-word ()
  '(signed-byte 22))
