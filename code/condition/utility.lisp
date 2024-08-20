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
      (quaviver:triple-float nil float-type 2 0 0 sign))))

(defun floating-point-overflow (float-type sign operation &rest operands)
  (restart-case
      (error 'cl:floating-point-overflow
             :operation operation
             :operands operands)
    (recover ()
      :report (lambda (stream)
                (format stream
                        #+clisp "Recover using the most ~:[negative~;positive~] floating point as the value."
                        #-clisp "Recover using ~:[negative~;positive~] infinity as the value."
                        (plusp sign)))
      #+clisp (if (minusp sign)
                  (ecase float-type
                    (short-float most-negative-short-float)
                    (single-float most-negative-single-float)
                    (double-float most-negative-double-float)
                    (long-float most-negative-long-float))
                  (ecase float-type
                    (short-float most-positive-short-float)
                    (single-float most-positive-single-float)
                    (double-float most-positive-double-float)
                    (long-float most-positive-long-float)))
      #-clisp (quaviver:triple-float nil float-type 2 0 :infinity sign))))
