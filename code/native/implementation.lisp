(in-package #:quaviver/native)

(defclass benchmark-client () ())

#+(or abcl ccl clasp clisp cmucl ecl sbcl)
(defmethod quaviver:float-integer
    ((client benchmark-client) (base (eql 10)) value)
  #+abcl  (system::flonum-to-string (abs value))
  #+ccl   (ccl::flonum-to-string value)
  #+clisp (system::decode-float-decimal value t)
  #+clasp (core::float-to-digits nil value nil nil)
  #+cmucl (lisp::flonum-to-digits value)
  #+ecl   (si::float-to-digits nil value nil nil)
  #+sbcl  (sb-impl::flonum-to-digits value))

(defclass client () ())

#+(or abcl ccl clasp clisp cmucl ecl sbcl)
(defmethod quaviver:float-integer
    ((client client) (base (eql 10)) value)
  #+abcl
  (multiple-value-bind (digits digits-length leading-point
                        trailing-point position)
      (system::flonum-to-string (abs value))
    (declare (ignore leading-point trailing-point))
    (values (remove nil (map 'vector #'digit-char-p digits))
            (- position digits-length -1)
            (floor (float-sign value))))
  #+ccl
  (multiple-value-bind (digits sign exponent)
      (ccl::flonum-to-string value)
    (values (map 'vector #'digit-char-p digits)
            exponent
            sign))
  #+clisp
  (multiple-value-bind (digits k position sign)
      (system::decode-float-decimal value t)
    (declare (ignore k))
    (values (map 'vector #'digit-char-p digits)
            (- position (length digits))
            (floor (float-sign value))))
  #+(or clasp cmucl ecl sbcl)
  (multiple-value-bind (position digits)
      #+clasp (core::float-to-digits nil value nil nil)
      #+cmucl (lisp::flonum-to-digits value)
      #+ecl   (si::float-to-digits nil value nil nil)
      #+sbcl  (sb-impl::flonum-to-digits value)
    (values (map 'vector #'digit-char-p digits)
            (- position (length digits))
            (floor (float-sign value)))))
