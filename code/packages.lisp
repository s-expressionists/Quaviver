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
           #:exponent-bytespec
           #:sign-bytespec
           #:nan-payload-bytespec
           #:nan-type-bytespec
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
