(in-package #:quaviver.condition)

(defun floating-point-underflow (float-type sign operation &rest operands)
  (restart-case
      (error 'cl:floating-point-underflow
             :operation operation
             :operands operands)
    (recover ()
      :report (lambda (stream)
                (format stream "Recover using ~:[negative~;positive~] zero as the value."
                        (plusp sign)))
      (quaviver:triple-float nil float-type
                             (quaviver:primitive-base float-type)
                             0 0 sign))))

(defun floating-point-overflow (float-type sign operation &rest operands)
  (restart-case
      (error 'cl:floating-point-overflow
             :operation operation
             :operands operands)
    (recover ()
      :report (lambda (stream)
                (format stream
                        "Recover using~:[ the most~;~] ~:[negative~;positive~] ~:[floating point~;infinity~] as the value."
                        (quaviver:infinityp float-type)
                        (plusp sign)
                        (quaviver:infinityp float-type)))
      (cond ((quaviver:infinityp float-type)
             (quaviver:triple-float nil float-type
                                    (quaviver:primitive-base float-type)
                                    0 :infinity sign))
            ((minusp sign)
             (ecase float-type
               (short-float (symbol-value 'most-negative-short-float))
               (single-float (symbol-value 'most-negative-single-float))
               (double-float (symbol-value 'most-negative-double-float))
               (long-float (symbol-value 'most-negative-long-float))))
            (t
             (ecase float-type
               (short-float (symbol-value 'most-positive-short-float))
               (single-float (symbol-value 'most-positive-single-float))
               (double-float (symbol-value 'most-positive-double-float))
               (long-float (symbol-value 'most-positive-long-float))))))))
