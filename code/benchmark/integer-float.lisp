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

(defvar *ieee754-client* (make-instance 'quaviver/ieee754:client))

(defvar *schubfach-client* (make-instance 'quaviver/schubfach:client))

(defun random-float (type)
  (multiple-value-list
   (quaviver:float-integer *schubfach-client* 10
                           (quaviver:bits-float *ieee754-client*
                                                type
                                                (random (ash 1 (quaviver:storage-size type)))))))

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
