;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT

(cl:in-package #:quaviver/compare)

(defclass integer-float ()
  ((name :reader test-name
         :initarg :name
         :initform nil)
   (client1 :accessor client1
            :initarg :client1)
   (client2 :accessor client2
            :initarg :client2)
   (base :accessor base
         :initarg :base)))

(defun compare/integer-float (test iterator)
  (with-accessors ((client1 client1)
                   (client2 client2)
                   (base base))
      test
    (let ((float-type (float-type (iterator-interval iterator))))
      (multiple-value-bind (significand exponent sign)
          (iterator-integer iterator base)
        (let ((float1 (quaviver:integer-float client1 float-type base significand exponent sign))
              (float2 (quaviver:integer-float client2 float-type base significand exponent sign)))
        (unless (equalp float1 float2)
          (list (iterator-bits iterator)
                (list significand exponent sign)
                float1
                float2)))))))

(defmethod iterator-value-pass-p ((test integer-float) iterator stream)
  (handler-case
      (let ((result (compare/integer-float test iterator)))
        (when result
          (format stream "~:<#x~v,'0x ~e ~s ~s~:@>~%"
                  (list* (float-hex-digits (float-type (iterator-interval iterator)))
                         result)))
        (not result))
    (error (condition)
      (declare (ignore condition))
      (format stream "~:@<#x~x :error~:@>~%" (iterator-bits iterator))
      nil)))

(defun integer-float/j.l/f (&rest rest &key (coverage 1) &allow-other-keys)
  (apply #'test
         (list (make-instance 'integer-float
                              :client1 (make-instance 'quaviver/jaffer:client)
                              :client2 (make-instance 'quaviver/liebler:client)
                              :base 10))
         (list (make-instance 'bit-interval
                              :coverage coverage))
         rest))

(defun integer-float/j.l/d (&rest rest &key (coverage (expt 2 -32)) &allow-other-keys)
  (apply #'test
         (list (make-instance 'integer-float
                              :client1 (make-instance 'quaviver/jaffer:client)
                              :client2 (make-instance 'quaviver/liebler:client)
                              :base 10))
         (list (make-instance 'bit-interval
                              :float-type 'double-float
                              :coverage coverage))
         rest))
