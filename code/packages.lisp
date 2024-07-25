(defpackage #:quaviver
  (:use #:common-lisp)
  (:export #:bits-float
           #:bits-float-form
           #:float-bits
           #:float-bits-form
           #:integer-float
           #:internal-integer-float-form
           #:float-integer
           #:float-internal-integer-form
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
           #:subnormalp
           #:non-number-p
           #:internal-base
           #:exponent-bias
           #:max-exponent
           #:min-exponent
           #:arithmetic-size
           #:significand-word
           #:exponent-word))

#+clisp
(pushnew :quaviver/short-float *features*)

#+(and ecl long-float)
(pushnew :quaviver/long-float *features*)

(defpackage #:quaviver.math
  (:use #:common-lisp)
  (:shadow #:expt)
  (:export #:arithmetic-word
           #:hi/64
           #:hi/hi64/128
           #:expt/32-10
           #:expt/64-10
           #:expt/128-10
           #:expt/256-10
           #:expt
           #:round-to-odd/32
           #:round-to-odd/64
           #:round-to-odd/128
           #:round-to-odd/256
           #:round-to-odd
           #:floor-multiply/32-64q64
           #:floor-multiply/evenp/32-64q64
           #:floor-multiply/64-128q128
           #:floor-multiply/evenp/64-128q128
           #:floor-log-expt
           #:ceiling-log-expt
           #:count-digits))

#+sbcl
(pushnew :quaviver.math/smallnum *features*)

(defpackage #:quaviver.condition
  (:use #:common-lisp)
  (:shadow #:floating-point-overflow
           #:floating-point-underflow)
  (:export #:floating-point-overflow
           #:floating-point-underflow))
