(cl:in-package #:quaviver/benchmark)

(defvar *float-integer-tests*
  (list `(:type single-float :limit ,most-positive-single-float)
        `(:type double-float :limit ,most-positive-double-float)
        #+quaviver/long-float
        `(:type long-float   :limit ,most-positive-long-float)))

(defvar *float-integer-clients*
  `((:label "Burger-Dybvig"
     :initargs (quaviver/burger-dybvig:client)
     :types (single-float double-float long-float))
    (:label "Schubfach"
     :initargs (quaviver/schubfach:client)
     :types (single-float double-float long-float))
    #+(or abcl ccl clasp cmucl ecl sbcl)
    (:label "Native"
     :initargs (quaviver/native:benchmark-client)
     :types (single-float double-float long-float))))

(defun float-integer (&key (base 10)
                           (name (uiop:implementation-identifier)))
  (let ((results (bench (lambda (client &key type limit)
                          (quaviver:float-integer client
                                                  base
                                                  (* (1- (ash (random 2) 1))
                                                     (random limit))))
                        *float-integer-tests*
                        *float-integer-clients*)))
    (write-results name `(quaviver:float-integer ,base) results)
    (report/run-summary "float-integer" *float-integer-tests* results)))
