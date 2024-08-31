(cl:in-package #:quaviver/benchmark)

(defvar *float-triple-tests*
  (list #+quaviver/short-float
        `(:type short-float :limit ,most-positive-short-float)
        `(:type single-float :limit ,most-positive-single-float)
        `(:type double-float :limit ,most-positive-double-float)
        #+quaviver/long-float
        `(:type long-float   :limit ,most-positive-long-float)))

(defvar *float-triple-clients*
  `((:label "Burger-Dybvig"
     :initargs (quaviver/burger-dybvig:client)
     :types (short-float single-float double-float long-float))
    (:label "Schubfach"
     :initargs (quaviver/schubfach:client)
     :types (short-float single-float double-float long-float))
    (:label "Dragonbox"
     :initargs (quaviver/dragonbox:nearest-client)
     :types (short-float single-float double-float))
    #+(or abcl ccl clasp cmucl ecl sbcl)
    (:label "Native"
     :initargs (quaviver/native:benchmark-client)
     :types (short-float single-float double-float long-float))))

(defun float-triple (&key (base 10)
                          (name (uiop:implementation-identifier)))
  (let ((results (bench (lambda (client &key type limit)
                          (declare (ignore type))
                          (quaviver:float-triple client
                                                 base
                                                 (* (1- (ash (random 2) 1))
                                                    (random limit))))
                        *float-triple-tests*
                        *float-triple-clients*)))
    (write-results name `(quaviver:float-triple ,base) results)
    (report/run-summary "float-triple" *float-triple-tests* results)))
