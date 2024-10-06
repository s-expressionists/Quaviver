(defpackage #:quaviver
  (:use #:common-lisp)
  (:export #:arithmetic-size
           #:bits-float
           #:bits-float-form
           #:bits-primitive-triple
           #:bits-primitive-triple-form
           #:compose-digits
           #:define-number-parser
           #:exact-implementation-type-p
           #:exponent-bias
           #:exponent-byte-form
           #:exponent-bytespec
           #:exponent-size
           #:exponent-word
           #:external-type
           #:float-bits
           #:float-bits-form
           #:float-primitive-triple-form
           #:float-triple
           #:hidden-bit-p
           #:implementation-type
           #:infinityp
           #:max-exponent
           #:min-exponent
           #:nan-payload-byte-form
           #:nan-payload-bytespec
           #:nan-type-byte-form
           #:nan-type-bytespec
           #:non-number-p
           #:parse-digits
           #:parse-number
           #:primitive-base
           #:primitive-triple-bits
           #:primitive-triple-bits-form
           #:primitive-triple-float-form
           #:read-digits
           #:read-number
           #:sign-byte-form
           #:sign-bytespec
           #:sign-size
           #:significand-byte-form
           #:significand-bytespec
           #:significand-size
           #:significand-word
           #:storage-size
           #:subnormalp
           #:triple-float
           #:write-digits
           #:write-number))

#+(or clisp (and clasp short-float))
(pushnew :quaviver/short-float *features*)

#+(and (or clasp ecl) long-float)
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
           #:floating-point-underflow
           #:assertion-failed-error
           #:invalid-character-error
           #:invalid-leading-zeros-error
           #:invalid-property-error
           #:missing-digits-error
           #:recover))
