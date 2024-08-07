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

(in-package #:quaviver.math)

(deftype arithmetic-word (arithmetic-size &optional (count 1))
  #+quaviver.math/smallnum
  (case arithmetic-size
    (32 (case count
          (1 `(unsigned-byte 32))
          (2 `(unsigned-byte 64))
          (otherwise `(simple-array (unsigned-byte 64) (,(ceiling count 2))))))
    (64 (if (eql count 1)
            `(unsigned-byte 64)
            `(simple-array (unsigned-byte 64) (,count))))
    (otherwise `(unsigned-byte ,(* arithmetic-size count))))
  #-quaviver.math/smallnum
  `(unsigned-byte ,(* arithmetic-size count)))

;;; Low-level, smallnum-optimized arithmetic operations
;;;
;;; These operations represent bignums as individual ub64 values.
;;; They are not exported from quaviver.math.
;;;
;;; Some of these operations are ported from Dragonbox.

(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 64))
                          (values (unsigned-byte 64) &optional))
                +/64-64)
         (ftype (function ((unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64))
                          (values (unsigned-byte 64) (unsigned-byte 64) &optional))
                +/128-64)
         (ftype (function ((unsigned-byte 32) (unsigned-byte 64))
                          (values (unsigned-byte 64) &optional))
                */32-64/hi64
                */32-64/lo64)
         (ftype (function ((unsigned-byte 64) (unsigned-byte 64))
                          (values (unsigned-byte 64) (unsigned-byte 64) &optional))
                */64-64)
         (ftype (function ((unsigned-byte 64) (unsigned-byte 64))
                          (values (unsigned-byte 64) &optional))
                */64-64/hi64
                */64-64/lo64)
         (ftype (function ((unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64))
                          (values (unsigned-byte 64) (unsigned-byte 64) &optional))
                */64-128/hi128
                */64-128/lo128)
         (inline +/64-64
                 +/128-64
                 */32-64/hi64
                 */32-64/lo64
                 */64-64
                 */64-64/hi64
                 */64-64/lo64
                 */64-128/hi128
                 */64-128/lo128))

(defun +/64-64 (x y)
  (ldb (byte 64 0) (+ x y)))

;;; Based on https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L646-L650
(defun +/128-64 (xh xl y)
  (let ((rl (+/64-64 xl y)))
    (values (+/64-64 xh (if (< rl xl) 1 0))
            rl)))

;;; Based on https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L835-L848
(defun */32-64/hi64 (x y)
  (+/64-64 (* x (ldb (byte 32 32) y))
           (ash (* x (ldb (byte 32 0) y))
                -32)))

(defun */32-64/lo64 (x y)
  ;; SBCL elides bignums.
  (ldb (byte 64 0) (* x y)))

;;; Based on https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L737-L753
(defun */64-64 (x y)
  (let* ((a (ldb (byte 32 32) x))
         (b (ldb (byte 32 0) x))
         (c (ldb (byte 32 32) y))
         (d (ldb (byte 32 0) y))
         (ac (* a c))
         (bc (* b c))
         (ad (* a d))
         (bd (* b d))
         (u (+ (ldb (byte 32 32) bd)
               (ldb (byte 32 0) ad)
               (ldb (byte 32 0) bc))))
    (declare ((unsigned-byte 32) a b c d)
             ((unsigned-byte 64) ac bc ad bd u))
    ;; (+ (ash ac 64) (ash ad 32) (ash bc 32) bd)
    (values (ldb (byte 64 0)
                 (+ ac
                    (ldb (byte 32 32) u)
                    (ldb (byte 32 32) ad)
                    (ldb (byte 32 32) bc)))
            (+/64-64 (ash (ldb (byte 32 0) u)
                          32)
                     (ldb (byte 32 0) bd)))))

;;; Based on https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L783-L798
(defun */64-64/hi64 (x y)
  (let* ((a (ldb (byte 32 32) x))
         (b (ldb (byte 32 0) x))
         (c (ldb (byte 32 32) y))
         (d (ldb (byte 32 0) y))
         (ac (* a c))
         (bc (* b c))
         (ad (* a d))
         (bd (* b d))
         (u (+ (ldb (byte 32 32) bd)
               (ldb (byte 32 0) ad)
               (ldb (byte 32 0) bc))))
    (declare ((unsigned-byte 32) a b c d)
             ((unsigned-byte 64) ac bc ad bd u))
    ;; (+ (ash ac 64) (ash ad 32) (ash bc 32) bd)
    (ldb (byte 64 0)
         (+ ac
            (ldb (byte 32 32) u)
            (ldb (byte 32 32) ad)
            (ldb (byte 32 32) bc)))))

(defun */64-64/lo64 (x y)
  ;; SBCL elides bignums.
  (ldb (byte 64 0) (* x y)))

;;; Based on https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L826-L831
(defun */64-128/hi128 (x yh yl)
  (multiple-value-bind (rh rl) (*/64-64 x yh)
    (+/128-64 rh rl (*/64-64/hi64 x yl))))

;;; Based on https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L852-L857
(defun */64-128/lo128 (x yh yl)
  (multiple-value-bind (rh rl) (*/64-64 x yl)
    (values (+/64-64 (*/64-64/lo64 x yh)
                     rh)
            rl)))

;;; High-level arithmetic operations

(declaim (ftype (function ((arithmetic-word 64) (integer 0 64))
                          (values (arithmetic-word 64) &optional))
                hi/64)
         (ftype (function ((arithmetic-word 64 2) (integer 0 64))
                          (values (arithmetic-word 64) &optional))
                hi/hi64/128)
         (ftype (function ((arithmetic-word 32) (arithmetic-word 32 2) &optional (integer 0 (32)))
                          (values (arithmetic-word 32) boolean &optional))
                floor-multiply/32-64q64)
         (ftype (function ((arithmetic-word 32) (arithmetic-word 32 2) &optional (integer 0 (32)))
                          (values boolean boolean &optional))
                floor-multiply/evenp/32-64q64)
         (ftype (function ((arithmetic-word 64) (arithmetic-word 64 2) &optional (integer 0 (64)))
                          (values (arithmetic-word 64) boolean &optional))
                floor-multiply/64-128q128)
         (ftype (function ((arithmetic-word 64) (arithmetic-word 64 2) &optional (integer 0 (64)))
                          (values boolean boolean &optional))
                floor-multiply/evenp/64-128q128)
         (inline hi/64
                 hi/hi64/128
                 floor-multiply/32-64q64
                 floor-multiply/evenp/32-64q64
                 floor-multiply/64-128q128
                 floor-multiply/evenp/64-128q128))

(defun hi/64 (x count)
  (ash x (- count 64)))

(define-compiler-macro hi/64 (&whole whole x count)
  (case count
    (0 0)
    (64 `(the (arithmetic-word 64) ,x))
    (otherwise whole)))

(defun hi/hi64/128 (x count)
  #+quaviver.math/smallnum
  (hi/64 (aref x 0) count)
  #-quaviver.math/smallnum
  (ash x (- count 128)))

(define-compiler-macro hi/hi64/128 (&whole whole x count)
  #-quaviver.math/smallnum
  (declare (ignore x))
  (case count
    (0 0)
    #+quaviver.math/smallnum
    (64 `(aref (the (arithmetic-word 64 2) ,x) 0))
    (otherwise whole)))

;;; The FLOOR-MULTIPLY operations compute
;;;
;;;   floor(2^pre * x * y)
;;;
;;; and return a second value which indicates whether the result is an
;;; integer.
;;; They require that 2^pre * x not exceed the size of x.
;;;
;;; Based on
;;; https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L3064-L3068
;;; and
;;; https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L3121-L3125

(defmacro %floor-multiply/n-2nq2n (x y pre-shift size)
  `(let ((r (* (the (arithmetic-word ,size) (ash ,x ,pre-shift))
               ,y)))
     (values (ldb (byte ,size ,(* size 2)) r)      ; integer part
             (zerop (ldb (byte ,size ,size) r))))) ; integer-p

(defun floor-multiply/32-64q64 (x y &optional (pre-shift 0))
  #+quaviver.math/smallnum
  (let ((r (*/32-64/hi64 (ash x pre-shift) y)))
    (values (ldb (byte 32 32) r)             ; integer part
            (not (ldb-test (byte 32 0) r)))) ; integer-p
  #-quaviver.math/smallnum
  (%floor-multiply/n-2nq2n x y pre-shift 32))

(defun floor-multiply/64-128q128 (x y &optional (pre-shift 0))
  #+quaviver.math/smallnum
  (multiple-value-bind (rh rl)
      (*/64-128/hi128 (ash x pre-shift) (aref y 0) (aref y 1))
    (values rh (zerop rl))) ; integer part, integer-p
  #-quaviver.math/smallnum
  (%floor-multiply/n-2nq2n x y pre-shift 64))

;;; The FLOOR-MULTIPLY/EVENP operations compute the same thing as
;;; FLOOR-MULTIPLY, but return as first value whether the result is even
;;; instead of the result itself.
;;;
;;; Based on
;;; https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L3076-L3085
;;; and
;;; https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L3134-L3144

(defmacro %floor-multiply/evenp/n-2nq2n (x y pre-shift size)
  `(let* ((r (* ,x ,y)))
     (values (logbitp (- ,(* size 2) ,pre-shift) r) ; even-p
             (zerop (ldb (byte ,size (- ,size ,pre-shift)) r))))) ; integer-p

(defun floor-multiply/evenp/32-64q64 (x y &optional (pre-shift 0))
  #+quaviver.math/smallnum
  (let ((r (*/32-64/lo64 x y)))
    (values (logbitp (- 64 pre-shift) r)                    ; even-p
            (not (ldb-test (byte 32 (- 32 pre-shift)) r)))) ; integer-p
  #-quaviver.math/smallnum
  (%floor-multiply/evenp/n-2nq2n x y pre-shift 32))

(defun floor-multiply/evenp/64-128q128 (x y &optional (pre-shift 0))
  #+quaviver.math/smallnum
  (multiple-value-bind (rh rl)
      (*/64-128/lo128 x (aref y 0) (aref y 1))
    (values (logbitp (- 64 pre-shift) rh) ; even-p
            (and (zerop (ldb (byte (- 64 pre-shift) pre-shift) rh)) ; integer-p
                 (zerop (ldb (byte 64 (- 64 pre-shift)) rl)))))
  #-quaviver.math/smallnum
  (%floor-multiply/evenp/n-2nq2n x y pre-shift 64))
