;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT AND (Apache-2.0 WITH LLVM-exception OR BSL-1.0)
;;;;
;;;; This file contains code ported from Dragonbox [1], which at the
;;;; time of the port was copyright 2020–2024 Junekey Jeon and licensed
;;;; under Apache-2.0 WITH LLVM-exception OR BSL-1.0.
;;;;
;;;; Any original code herein is licensed under the MIT license (Expat).
;;;;
;;;; [1]: https://github.com/jk-jeon/dragonbox

(in-package #:quaviver/trailing-zeros)

;;; Trailing zeros

(declaim (inline rotr/32))
(defun rotr/32 (count integer)
  (declare (optimize speed)
           ((integer 0 (32)) count)
           ((unsigned-byte 32) integer))
  #+sbcl
  (sb-rotate-byte:rotate-byte (- count) (byte 32 0) integer)
  #-sbcl
  (logior (ash integer (- count))
          (ldb (byte 32 0) (ash integer (- 32 count)))))

(declaim (inline rotr/64))
(defun rotr/64 (count integer)
  (declare (optimize speed)
           ((integer 0 (64)) count)
           ((unsigned-byte 64) integer))
  #+sbcl
  (sb-rotate-byte:rotate-byte (- count) (byte 64 0) integer)
  #-sbcl
  (logior (ash integer (- count))
          (ldb (byte 64 0) (ash integer (- 64 count)))))

(declaim (inline remove-trailing-zeros/32))
(defun remove-trailing-zeros/32 (significand exponent sign)
  (declare (optimize speed)
           (type (unsigned-byte 32) significand)
           (type (or keyword fixnum) exponent))
  (unless (or (not (numberp exponent))
              (zerop significand))
    ;; Remove trailing zeros from significand; ported from [1].
    ;;
    ;; [1]: https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L2950
    (let ((r 0)
          (b nil)
          (s 0))
      (declare ((unsigned-byte 32) r))
      (setf r (rotr/32 4 (ldb (byte 32 0) (* significand 184254097)))
            b (< r 429497)
            s (if b 1 0)
            significand (if b r significand)
            r (rotr/32 2 (ldb (byte 32 0) (* significand 42949673)))
            b (< r 42949673)
            s (+ (* s 2) (if b 1 0))
            significand (if b r significand)
            r (rotr/32 1 (ldb (byte 32 0) (* significand 1288490189)))
            b (< r 429496730)
            s (+ (* s 2) (if b 1 0))
            significand (if b r significand)
            exponent (+ exponent s))))
  (values significand exponent sign))

(declaim (inline remove-trailing-zeros/64))
(defun remove-trailing-zeros/64 (significand exponent sign)
  (declare (optimize speed)
           ((unsigned-byte 64) significand)
           (type (or keyword fixnum) exponent))
  (unless (or (not (numberp exponent))
              (zerop significand))
    ;; Remove trailing zeros from significand; ported from [1].
    ;;
    ;; [1]: https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L2982
    (let ((r 0)
          (b nil)
          (s 0))
      (declare ((unsigned-byte 64) r))
      (setf r (rotr/64 8 (ldb (byte 64 0) (* significand 28999941890838049)))
            b (< r 184467440738)
            s (if b 1 0)
            significand (if b r significand)
            r (rotr/64 4 (ldb (byte 64 0) (* significand 182622766329724561)))
            b (< r 1844674407370956)
            s (+ (* s 2) (if b 1 0))
            significand (if b r significand)
            r (rotr/64 2 (ldb (byte 64 0) (* significand 10330176681277348905)))
            b (< r 184467440737095517)
            s (+ (* s 2) (if b 1 0))
            significand (if b r significand)
            r (rotr/64 1 (ldb (byte 64 0) (* significand 14757395258967641293)))
            b (< r 1844674407370955162)
            s (+ (* s 2) (if b 1 0))
            significand (if b r significand)
            exponent (+ exponent s))))
  (values significand exponent sign))

(declaim (inline remove-trailing-zeros/n))
(defun remove-trailing-zeros/n (significand exponent sign)
  (unless (or (not (numberp exponent))
              (zerop significand))
    (prog (quotient remainder)
     next
       (multiple-value-setq (quotient remainder)
         (floor significand 10))
       (when (zerop remainder)
         (setf significand quotient)
         (incf exponent)
         (go next))))
  (values significand exponent sign))

(defmacro remove-trailing-zeros (type)
  (with-accessors ((arithmetic-size quaviver:arithmetic-size))
      type
    (case arithmetic-size
      (32
       `(multiple-value-call #'remove-trailing-zeros/32 (call-next-method)))
      (64
       `(multiple-value-call #'remove-trailing-zeros/64 (call-next-method)))
      (otherwise
       `(multiple-value-call #'remove-trailing-zeros/n (call-next-method))))))

;;; Client

(defclass client () ())

#+clisp
(defmethod quaviver:float-integer :around
    ((client client) (base (eql 10)) value)
  (typecase value
    #+quaviver/short-float
    (short-float
     (remove-trailing-zeros short-float))
    (single-float
     (remove-trailing-zeros single-float))
    #+quaviver/long-float
    (double-float
     (remove-trailing-zeros double-float))
    (otherwise
     (call-next-method))))

#+(and (not clisp) quaviver/short-float)
(defmethod quaviver:float-integer :around
    ((client client) (base (eql 10)) (value short-float))
  (remove-trailing-zeros short-float))

#-clisp
(defmethod quaviver:float-integer :around
    ((client client) (base (eql 10)) (value single-float))
  (remove-trailing-zeros single-float))

#-clisp
(defmethod quaviver:float-integer :around
    ((client client) (base (eql 10)) (value double-float))
  (remove-trailing-zeros double-float))

#+(and (not clisp) quaviver/long-float)
(defmethod quaviver:float-integer :around
    ((client client) (base (eql 10)) (value long-float))
  (remove-trailing-zeros long-float))
