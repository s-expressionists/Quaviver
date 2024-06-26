(cl:in-package #:quaviver/benchmark)

(defvar *integer-float-tests*
  (list `(:type single-float)
        `(:type double-float)
        #+quaviver/long-float
        `(:type long-float)))

(defvar *integer-float-clients*
  `((:label "Jaffer"
     :initargs (quaviver/jaffer:client)
     :types (single-float double-float long-float))
    (:label "Liebler"
     :initargs (quaviver/liebler:client)
     :types (single-float double-float long-float))))

(defun random-float (type)
  (list (random (ash 1 (quaviver:significand-size type)))
        (quaviver/math:floor-log10-expt2 (+ (random (- (quaviver:max-exponent type)
                                                       (quaviver:min-exponent type)
                                                       (quaviver:significand-size type)
                                                       4))
                                            (quaviver:min-exponent type)
                                            (quaviver:significand-size type)
                                            2))
        (if (zerop (random 2)) 1 -1)))

(defun integer-float (&key (base 10)
                           (name (uiop:implementation-identifier)))
  (let ((results (bench (lambda (client &key type)
                          (apply #'quaviver:integer-float
                                 client type base
                                 (random-float type)))
                        *integer-float-tests*
                        *integer-float-clients*)))
    (write-results name `(quaviver:integer-float ,base) results)
    (report/run-summary "integer-float" *integer-float-tests* results)))
