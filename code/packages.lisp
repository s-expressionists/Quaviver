(defpackage #:quaviver
  (:use #:common-lisp)
  (:export #:bits-float
           #:float-bits
           #:integer-float
           #:float-integer
           #:digits-integer
           #:integer-digits
           #:storage-size
           #:significand-bytespec
           #:significand-byte-form
           #:significand-size
           #:exponent-bytespec
           #:exponent-byte-form
           #:exponent-size
           #:sign-bytespec
           #:sign-byte-form
           #:sign-size
           #:nan-payload-bytespec
           #:nan-payload-byte-form
           #:nan-type-bytespec
           #:nan-type-byte-form
           #:hidden-bit-p
           #:exponent-bias
           #:max-exponent
           #:min-exponent
           #:arithmetic-size))

#+(and ecl long-float)
(pushnew :quaviver/long-float *features*)
