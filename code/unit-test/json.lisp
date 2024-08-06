(in-package #:quaviver/unit-test)

(defclass json-client (quaviver/json:client) ())

(defmethod quaviver:triple-float ((client json-client) float-type base significand exponent sign)
  (list significand exponent sign))

(define-test json

  (define-test json.n_number_++
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "++1234")))

  (define-test json.n_number_+1
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "+1")))

  (define-test json.n_number_+Inf
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "+Inf")))

  (define-test json.n_number_-01
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "-01")))

  (define-test json.n_number_-2.
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "-2.")))

  (define-test json.n_number_-NaN
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "-NaN")))

  (define-test json.n_number_.-1
    (fail (quaviver:parse-number (make-instance 'json-client) 10 ".-1")))

  (define-test json.n_number_.2e-3
    (fail (quaviver:parse-number (make-instance 'json-client) 10 ".2e-3")))

  (define-test json.n_number_0.3e+
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "0.3e+")))

  (define-test json.n_number_0.3e
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "0.3e")))

  (define-test json.n_number_0.e1
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "0.e1")))

  (define-test json.n_number_0_capital_E+
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "0E+")))

  (define-test json.n_number_0_capital_E
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "0E")))

  (define-test json.n_number_0e+
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "0e+")))

  (define-test json.n_number_0e
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "0e")))

  (define-test json.n_number_1.0e+
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "1.0e+")))

  (define-test json.n_number_1.0e-
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "1.0e-")))

  (define-test json.n_number_1.0e
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "1.0e")))

  (define-test json.n_number_1eE2
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "1eE2")))

  (define-test json.n_number_2.e+3
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "2.e+3")))

  (define-test json.n_number_2.e-3
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "2.e-3")))

  (define-test json.n_number_2.e3
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "2.e3")))

  (define-test json.n_number_9.e+
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "9.e+")))

  (define-test json.n_number_Inf
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "Inf")))

  (define-test json.n_number_NaN
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "NaN")))

  (define-test json.n_number_infinity
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "Infinity")))

  (define-test json.n_number_invalid+-
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "0e+-1")))

  (define-test json.n_number_minus_infinity
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "-Infinity")))

  (define-test json.n_number_minus_sign_with_trailing_garbage
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "-foo")))

  (define-test json.n_number_minus_space_1
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "- 1")))

  (define-test json.n_number_neg_int_starting_with_zero
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "-012")))

  (define-test json.n_number_neg_real_without_int_part
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "-.123")))

  (define-test json.n_number_real_garbage_after_e
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "1ea")))

  (define-test json.n_number_real_without_fractional_part
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "1.")))

  (define-test json.n_number_starting_with_dot
    (fail (quaviver:parse-number (make-instance 'json-client) 10 ".123")))

  (define-test json.n_number_with_leading_zero
    (fail (quaviver:parse-number (make-instance 'json-client) 10 "012")))

  (define-test json.y_number
    (is equalp
        (list 123 65 1)
        (quaviver:parse-number (make-instance 'json-client) 10 "123e65")))

  (define-test json.y_number_0e+1
    (is equalp
        (list 0 1 1)
        (quaviver:parse-number (make-instance 'json-client) 10 "0e+1")))

  (define-test json.y_number_0e1
    (is equalp
        (list 0 1 1)
        (quaviver:parse-number (make-instance 'json-client) 10 "0e1")))

  (define-test json.y_number_double_close_to_zero
    (is equalp
        (list 1 -78 -1)
        (quaviver:parse-number (make-instance 'json-client) 10 "-0.000000000000000000000000000000000000000000000000000000000000000000000000000001
")))

  (define-test json.y_number_int_with_exp
    (is equalp
        (list 20 1 1)
        (quaviver:parse-number (make-instance 'json-client) 10 "20e1")))

  (define-test json.y_number_minus_zero
    (is equalp
        (list 0 0 -1)
        (quaviver:parse-number (make-instance 'json-client) 10 "-0"
                       0 2 nil)))

  (define-test json.y_number_negative_int
    (is equalp
        (list 123 0 -1)
        (quaviver:parse-number (make-instance 'json-client) 10 "-123"
                       0 4 nil)))

  (define-test json.y_number_negative_one
    (is equalp
        (list 1 0 -1)
        (quaviver:parse-number (make-instance 'json-client) 10 "-1"
                       0 2 nil)))

  (define-test json.y_number_real_capital_e
    (is equalp
        (list 1 22 1)
        (quaviver:parse-number (make-instance 'json-client) 10 "1E22")))

  (define-test json.y_number_real_capital_e_neg_exp
    (is equalp
        (list 1 -2 1)
        (quaviver:parse-number (make-instance 'json-client) 10 "1E-2")))

  (define-test json.y_number_real_capital_e_pos_exp
    (is equalp
        (list 1 2 1)
        (quaviver:parse-number (make-instance 'json-client) 10 "1E+2")))

  (define-test json.y_number_real_exponent
    (is equalp
        (list 123 45 1)
        (quaviver:parse-number (make-instance 'json-client) 10 "123e45")))

  (define-test json.y_number_real_fraction_exponent
    (is equalp
        (list 123456 75 1)
        (quaviver:parse-number (make-instance 'json-client) 10 "123.456e78")))

  (define-test json.y_number_real_neg_exp
    (is equalp
        (list 1 -2 1)
        (quaviver:parse-number (make-instance 'json-client) 10 "1e-2")))

  (define-test json.y_number_real_pos_exponent
    (is equalp
        (list 1 2 1)
        (quaviver:parse-number (make-instance 'json-client) 10 "1e+2")))

  (define-test json.y_number_simple_int
    (is equalp
        (list 123 0 1)
        (quaviver:parse-number (make-instance 'json-client) 10 "123"
                       0 3 nil)))

  (define-test json.y_number_simple_real
    (is equalp
        (list 123456789 -6 1)
        (quaviver:parse-number (make-instance 'json-client) 10 "123.456789"))))
