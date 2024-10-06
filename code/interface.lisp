(cl:in-package #:quaviver)

(defgeneric bits-float (float-type bits))

(defgeneric bits-float-form (float-type bits))

(defgeneric float-bits (float-type value))

(defgeneric float-bits-form (float-type value))

(defgeneric triple-float (client float-type base significand exponent sign))

(defgeneric primitive-triple-float-form (float-type significand exponent sign))

(defgeneric float-triple (client base value))

(defgeneric float-primitive-triple-form (float-type value))

(defgeneric bits-primitive-triple-form (float-type value))

(defgeneric bits-primitive-triple (float-type value))

(defgeneric primitive-triple-bits-form (float-type significand exponent sign))

(defgeneric primitive-triple-bits (float-type significand exponent sign))

(defgeneric parse-number (client base sequence
                          &optional start end integerp ratiop floatp float-type))

(defgeneric read-number (client base stream
                         &optional integerp ratiop floatp float-type))

(defgeneric write-number (client base value stream))

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

(defgeneric infinityp (type))

(defgeneric primitive-base (type))

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

(defgeneric implementation-type (type)
  (:method ((type (eql 'short-float)))
    #+quaviver/short-float 'short-float
    #-quaviver/short-float 'single-float)
  (:method ((type (eql 'single-float)))
    'single-float)
  (:method ((type (eql 'double-float)))
    'double-float)
  (:method ((type (eql 'long-float)))
    #+quaviver/long-float 'long-float
    #-quaviver/long-float 'double-float))

(defgeneric exact-implementation-type-p (type)
  (:method ((type (eql 'short-float)))
    t)
  (:method ((type (eql 'single-float)))
    t)
  (:method ((type (eql 'double-float)))
    t)
  (:method ((type (eql 'long-float)))
    t))
            
(defgeneric external-type (type)
  (:method (type)
    (declare (ignore type))
     nil)
  #-quaviver/short-float
  (:method ((type (eql 'short-float)))
    (external-type 'single-float))
  #-quaviver/long-float
  (:method ((type (eql 'long-float)))
    (external-type 'long-float)))
  
(deftype significand-word (type &optional (extra 0))
  `(unsigned-byte ,(+ (significand-size type) extra)))

(deftype exponent-word ()
  '(signed-byte 22))
