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
           #:exponent-bytespec
           #:exponent-byte-form
           #:sign-bytespec
           #:sign-byte-form
           #:nan-payload-bytespec
           #:nan-payload-byte-form
           #:nan-type-bytespec
           #:nan-type-byte-form
           #:hidden-bit-p
           #:exponent-bias
           #:max-exponent
           #:min-exponent
           #:significand-size
           #:arithmetic-size))

#+(and ecl long-float)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case
      (system:long-float-bits 0l0)
    (error (condition)
      (declare (ignore condition)))
    (:no-error (result)
      (declare (ignore result))
      (pushnew :quaviver/long-float *features*))))
