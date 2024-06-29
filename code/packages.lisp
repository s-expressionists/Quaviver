#+(and ecl long-float)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :quaviver/long-float *features*)
  (handler-case
      (system:long-float-bits 0l0)
    (error (condition)
      (declare (ignore condition))
      (pushnew :quaviver.bits/long-float *features*))))

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

(defpackage #:quaviver.bits
  (:use #:common-lisp)
  (:export #:bits-long-float
           #:long-float-bits))
