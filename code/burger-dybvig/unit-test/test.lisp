(cl:in-package #:quaviver/burger-dybvig/unit-test)

;;; This function goes through all single floats and checks that for a
;;; given float x, the successor of the predecessor x is x, as defined
;;; by the two preceding functions.
(define-test predecessor-successor.01
  (false (loop for x = most-positive-single-float then (quaviver/burger-dybvig::predecessor x)
               for y = (quaviver/burger-dybvig::predecessor x)
               for z = (quaviver/burger-dybvig::successor y)
               while (plusp y)
               unless (eql x (quaviver/burger-dybvig::successor y))
                 collect (x y z))))
