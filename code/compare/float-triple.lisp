;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT

(cl:in-package #:quaviver/compare)

(defclass float-triple ()
  ((name :reader test-name
         :initarg :name
         :initform nil)
   (client1 :accessor client1
            :initarg :client1)
   (client2 :accessor client2
            :initarg :client2)
   (base :accessor base
         :initarg :base)))

(defun compare/float-triple (test iterator)
  (with-accessors ((client1 client1)
                   (client2 client2)
                   (base base))
      test
    (let ((value (iterator-float iterator)))
      (multiple-value-bind (significand1 exponent1 sign1)
          (quaviver:float-triple client1 base value)
        (multiple-value-bind (significand2 exponent2 sign2)
            (quaviver:float-triple client2 base value)
          (unless (or (and (eql significand1 significand2) ; identical results
                           (eql exponent1 exponent2)
                           (eql sign1 sign2))
                      (and (eql sign1 sign2) ; significands have trailing zeros
                           (integerp significand2)
                           (integerp significand2)
                           (integerp exponent1)
                           (integerp exponent2)
                           (eql (* significand1
                                   (expt base
                                         (- exponent1
                                            (min exponent1 exponent2))))
                                (* significand2
                                   (expt base
                                         (- exponent2
                                            (min exponent1 exponent2)))))))
            (list (iterator-bits iterator)
                  value
                  (when significand1
                    (list significand1 exponent1 sign1))
                  (when significand2
                    (list significand2 exponent2 sign2)))))))))

(defmethod iterator-value-pass-p ((test float-triple) iterator stream)
  (handler-case
      (let ((result (compare/float-triple test iterator)))
        (when result
          (format stream "~:<#x~v,'0x ~e ~s ~s~:@>~%"
                  (list* (float-hex-digits (float-type (iterator-interval iterator)))
                         result)))
        (not result))
    (error (condition)
      ;; the condition is formatted separately to ensure it is READable.
      (format stream "~:@<#x~x :error ~s~:@>~%"
              (iterator-bits iterator)
              (format nil "~a" condition))
      nil)))

(defun float-triple/bd.s/f (&rest rest &key (coverage 1) &allow-other-keys)
  (apply #'test
         (list (make-instance 'float-triple
                              :client1 (make-instance 'quaviver/burger-dybvig:client)
                              :client2 (make-instance 'quaviver/schubfach:client)
                              :base 10))
         (list (make-instance 'bit-interval
                              :coverage coverage))
         rest))

(defun float-triple/bd.s/d (&rest rest &key (coverage (expt 2 -32)) &allow-other-keys)
  (apply #'test
         (list (make-instance 'float-triple
                              :client1 (make-instance 'quaviver/burger-dybvig:client)
                              :client2 (make-instance 'quaviver/schubfach:client)
                              :base 10))
         (list (make-instance 'bit-interval
                              :float-type 'double-float
                              :coverage coverage))
         rest))

(defun float-triple/s.d/f (&rest rest
                            &key (coverage 1) (rounding :away-from-zero)
                            &allow-other-keys)
  (apply #'test
         (list (make-instance 'float-triple
                              :client1 (make-instance 'quaviver/schubfach:client
                                                      :rounding rounding)
                              :client2 (make-instance 'quaviver/dragonbox:nearest-client
                                                      :binary-decimal-rounding rounding)
                              :base 10))
         (list (make-instance 'bit-interval
                              :coverage coverage))
         rest))

(defun float-triple/s.d/d (&rest rest
                            &key (coverage (expt 2 -32)) (rounding :away-from-zero)
                            &allow-other-keys)
  (apply #'test
         (list (make-instance 'float-triple
                              :client1 (make-instance 'quaviver/schubfach:client
                                                      :rounding rounding)
                              :client2 (make-instance 'quaviver/dragonbox:nearest-client
                                                      :binary-decimal-rounding rounding)
                              :base 10))
         (list (make-instance 'bit-interval
                              :float-type 'double-float
                              :coverage coverage))
         rest))
