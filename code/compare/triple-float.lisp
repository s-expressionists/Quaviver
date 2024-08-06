;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT

(cl:in-package #:quaviver/compare)

(defclass triple-float ()
  ((name :reader test-name
         :initarg :name
         :initform nil)
   (client1 :accessor client1
            :initarg :client1)
   (client2 :accessor client2
            :initarg :client2)
   (base :accessor base
         :initarg :base)))

(defun compare/triple-float (test iterator)
  (with-accessors ((client1 client1)
                   (client2 client2)
                   (base base))
      test
    (let ((float-type (float-type (iterator-interval iterator))))
      (multiple-value-bind (significand exponent sign)
          (handler-case
              (iterator-integer iterator base)
            (floating-point-invalid-operation ()
              ;; this might be signaled if we attempt to encode a signaling NaN.
              (return-from compare/triple-float nil)))
        (let ((float1 (quaviver:triple-float client1 float-type base significand exponent sign))
              (float2 (quaviver:triple-float client2 float-type base significand exponent sign)))
          (unless (equalp (multiple-value-list (quaviver:float-triple nil 2 float1))
                          (multiple-value-list (quaviver:float-triple nil 2 float2)))
            (list (iterator-bits iterator)
                  (list significand exponent sign)
                  float1
                  float2)))))))

(defmethod iterator-value-pass-p ((test triple-float) iterator stream)
  (handler-case
      (let ((result (compare/triple-float test iterator)))
        (when result
          (format stream "~:<#x~v,'0x ~e ~s ~s~:@>~%"
                  (list* (float-hex-digits (float-type (iterator-interval iterator)))
                         result)))
        (not result))
    (error (condition)
      (declare (ignore condition))
      ;; the condition is formatted separately to ensure it is READable.
      (format stream "~:@<#x~x :error ~s~:@>~%"
              (iterator-bits iterator)
              (format nil "~a" condition))
      nil)))

(defun triple-float/j.l/f (&rest rest &key (coverage 1) &allow-other-keys)
  (apply #'test
         (list (make-instance 'triple-float
                              :client1 (make-instance 'quaviver/jaffer:client)
                              :client2 (make-instance 'quaviver/liebler:client)
                              :base 10))
         (list (make-instance 'bit-interval
                              :coverage coverage))
         rest))

(defun triple-float/j.l/d (&rest rest &key (coverage (expt 2 -32)) &allow-other-keys)
  (apply #'test
         (list (make-instance 'triple-float
                              :client1 (make-instance 'quaviver/jaffer:client)
                              :client2 (make-instance 'quaviver/liebler:client)
                              :base 10))
         (list (make-instance 'bit-interval
                              :float-type 'double-float
                              :coverage coverage))
         rest))
