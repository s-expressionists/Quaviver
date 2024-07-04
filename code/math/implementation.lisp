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

(in-package #:quaviver/math)

(deftype arithmetic-word (arithmetic-size &optional (count 1))
  #+quaviver/math/smallnum
  (ecase arithmetic-size
    (32 (case count
          (1 `(unsigned-byte 32))
          (2 `(unsigned-byte 64))
          (otherwise `(simple-array (unsigned-byte 64) (,(ceiling count 2))))))
    (64 (if (eql count 1)
            `(unsigned-byte 64)
            `(simple-array (unsigned-byte 64) (,count))))
    (128 `(unsigned-byte ,(* arithmetic-size count))))
  #-quaviver/math/smallnum
  `(unsigned-byte ,(* arithmetic-size count)))

(declaim (ftype (function ((arithmetic-word 64) (integer 0 64))
                          (values (arithmetic-word 64) &optional))
                hi/64)
         (ftype (function ((arithmetic-word 64 2) (integer 0 64))
                          (values (arithmetic-word 64) &optional))
                hi/hi64/128)
         (ftype (function ((arithmetic-word 32) (arithmetic-word 32 2))
                          (values (arithmetic-word 32) &optional))
                round-to-odd/32-64)
         (ftype (function ((arithmetic-word 64) (arithmetic-word 64 2))
                          (values (arithmetic-word 64) &optional))
                round-to-odd/64-128)
         (ftype (function ((arithmetic-word 128) (arithmetic-word 128 2))
                          (values (arithmetic-word 128) &optional))
                round-to-odd/128-256)
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
         (ftype (function (fixnum)
                          (values (arithmetic-word 32 2) &optional))
                expt10/32)
         (ftype (function (fixnum)
                          (values (arithmetic-word 64 2) &optional))
                expt10/64)
         (ftype (function (fixnum)
                          (values (arithmetic-word 128 2) &optional))
                expt10/128)
         (ftype (function (fixnum fixnum fixnum &optional boolean)
                          (values fixnum &optional))
                floor-log-expt ceiling-log-expt)
         (inline hi/64
                 hi/hi64/128
                 round-to-odd/32-64
                 round-to-odd/64-128
                 round-to-odd/128-256
                 floor-multiply/32-64q64
                 floor-multiply/evenp/32-64q64
                 floor-multiply/64-128q128
                 floor-multiply/evenp/64-128q128
                 expt10/32
                 expt10/64
                 expt10/128
                 floor-log-expt
                 ceiling-log-expt))

(defun hi/64 (x count)
  (ash x (- count 64)))

(define-compiler-macro hi/64 (&whole whole x count)
  (case count
    (0 0)
    (64 `(the (arithmetic-word 64) ,x))
    (otherwise whole)))

(defun hi/hi64/128 (x count)
  (ash x (- count 128)))

(defmacro %round-to-odd-1 (cp g size)
  `(let ((p (* ,cp ,g)))
     (logior (ldb (byte ,size ,(ash size 1)) p)
             (if (> (ldb (byte ,size ,size) p) 1) 1 0))))

(defmacro %round-to-odd-2 (cp g size)
  `(let ((p (ash (* ,cp ,g) ,(- size))))
     (if (ldb-test (byte ,(1- size) 1) p)
         (logior (ash p ,(- size)) 1)
         (ash p ,(- size)))))

(defun round-to-odd/32-64 (cp g)
  #-(or ecl cmucl) (%round-to-odd-1 cp g 32)
  #+(or ecl cmucl) (%round-to-odd-2 cp g 32))

(defun round-to-odd/64-128 (cp g)
  (%round-to-odd-2 cp g 64))

(defun round-to-odd/128-256 (cp g)
  (%round-to-odd-2 cp g 128))

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
  (%floor-multiply/n-2nq2n x y pre-shift 32))

(defun floor-multiply/64-128q128 (x y &optional (pre-shift 0))
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
  (%floor-multiply/evenp/n-2nq2n x y pre-shift 32))

(defun floor-multiply/evenp/64-128q128 (x y &optional (pre-shift 0))
  (%floor-multiply/evenp/n-2nq2n x y pre-shift 64))

(defconstant +expt10/min-exponent/32+ -53)

(defconstant +expt10/max-exponent/32+ 53)

(defvar *expt10/values/32*
  (compute-expt +expt10/min-exponent/32+ +expt10/max-exponent/32+ 64))

(defun expt10/32 (power)
  (svref *expt10/values/32*
         (- (- +expt10/min-exponent/32+) power)))

(defconstant +expt10/min-exponent/64+ -342)

(defconstant +expt10/max-exponent/64+ 342)

(defvar *expt10/values/64*
  (compute-expt +expt10/min-exponent/64+ +expt10/max-exponent/64+ 128))

(defun expt10/64 (power)
  (svref *expt10/values/64*
         (- (- +expt10/min-exponent/64+) power)))

(defconstant +expt10/min-exponent/128+ -5023)

(defconstant +expt10/max-exponent/128+ 5023)

(defvar *expt10/values/128* nil)

(defun expt10/128 (power)
  (svref (or *expt10/values/128*
             (setf *expt10/values/128*
                   (compute-expt +expt10/min-exponent/128+ +expt10/max-exponent/128+ 256)))
         (- (- +expt10/min-exponent/128+) power)))

(defconstant +min-base+ 2)

(defconstant +max-base+ 36)

(defconstant +log-expt-shift+ 22)

(defvar *log-expt*
  (compute-log-expt +min-base+ +max-base+ +log-expt-shift+))

(defvar *log-3/4*
  (compute-log-3/4 +min-base+ +max-base+ +log-expt-shift+))

(defun floor-log-expt (log-base expt-base exp &optional three-quarters-p)
  (declare (optimize speed))
  (ash (+ (* exp (aref *log-expt*
                       (- log-base +min-base+)
                       (- expt-base +min-base+)))
          (if three-quarters-p
              (svref *log-3/4* (- log-base +min-base+))
              0))
       (- +log-expt-shift+)))

(define-compiler-macro floor-log-expt
    (&whole whole log-base expt-base exp &optional three-quarters-p)
  (if (or (not (constantp log-base))
          (not (constantp expt-base)))
      whole
      (let ((multiplier (aref *log-expt*
                              (- log-base +min-base+)
                              (- expt-base +min-base+)))
            (offset (svref *log-3/4* (- log-base +min-base+)))
            (shift (- +log-expt-shift+)))
        (cond ((null three-quarters-p)
               `(ash (* ,exp ,multiplier) ,shift))
              ((constantp three-quarters-p)
               `(ash (+ (* ,exp ,multiplier) ,offset)
                     ,shift))
              (t
               `(ash (+ (* ,exp ,multiplier)
                        (if ,three-quarters-p
                            ,offset
                            0))
                     ,shift))))))

(defun ceiling-log-expt (log-base expt-base exp &optional three-quarters-p)
  (values (ceiling (+ (* exp (aref *log-expt*
                                   (- log-base +min-base+)
                                   (- expt-base +min-base+)))
                      (if three-quarters-p
                          (svref *log-3/4* (- log-base +min-base+))
                          0))
                   (ash 1 +log-expt-shift+))))

(define-compiler-macro ceiling-log-expt
    (&whole whole log-base expt-base exp &optional three-quarters-p)
  (if (or (not (constantp log-base))
          (not (constantp expt-base)))
      whole
      (let ((multiplier (aref *log-expt*
                              (- log-base +min-base+)
                              (- expt-base +min-base+)))
            (offset (svref *log-3/4* (- log-base +min-base+)))
            (divisor (ash 1 +log-expt-shift+)))
        (cond ((null three-quarters-p)
               `(values (ceiling (* ,exp ,multiplier) ,divisor)))
              ((constantp three-quarters-p)
               `(values (ceiling (+ (* ,exp ,multiplier) ,offset)
                                 ,divisor)))
              (t
               `(values (ceiling (+ (* ,exp ,multiplier)
                                    (if ,three-quarters-p
                                        ,offset
                                        0))
                                 ,divisor)))))))
