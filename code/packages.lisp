(defpackage #:quaviver
  (:use #:common-lisp)
  (:export #:bits-float
           #:float-bits
           #:integer-float
           #:float-integer
           #:digits-integer
           #:integer-digits))

#+(and ecl long-float)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case
      (system:long-float-bits 0l0)
    (error (condition)
      (declare (ignore condition)))
    (:no-error (result)
      (declare (ignore result))
      (pushnew :quaviver/long-float *features*))))
