;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT AND (Apache-2.0 WITH LLVM-exception OR BSL-1.0)
;;;;
;;;; This file contains code ported from Dragonbox [1], which at the
;;;; time of the port was copyright 2020–2024 Junekey Jeon and licensed
;;;; under Apache-2.0 WITH LLVM-exception OR BSL-1.0.
;;;; This file also contains code ported from Daniel Lemire's blog [2],
;;;; which he has dedicated to the public domain.
;;;; This file also contains code ported from itoa-benchmark [3], which
;;;; at the time of the port was copyright 2014-2016 Milo Yip and
;;;; licensed under the MIT license (Expat).
;;;;
;;;; Any original code herein is licensed under the MIT license (Expat).
;;;;
;;;; [1]: https://github.com/jk-jeon/dragonbox
;;;; [2]: https://github.com/lemire/Code-used-on-Daniel-Lemire-s-blog/blob/693681a91167b0b694294bea35f5716c2d2ee264/2021/06/03
;;;; [3]: https://github.com/miloyip/itoa-benchmark

(in-package #:quaviver/integer-significand)

;;; Counting digits
;;;
;;; The digit-counting algorithms implemented here — basically faster
;;; versions of (CEILING (LOG INTEGER 10)) — include a port of Daniel
;;; Lemire's code [1,2] (which gives credit to Kendall Willets), and
;;; also a port of part of itoa-benchmark [3].
;;; An accompanying description of Daniel Lemire's algorithm is also
;;; available [4].
;;;
;;; The algorithms consist of computing the integer base-2 and base-10
;;; logarithms separately and then dividing them.
;;; The base-2 logarithm can be optimized to
;;; (1- (INTEGER-LENGTH INTEGER)) and the base-10 logarithm with lookup
;;; tables.
;;;
;;; Both COUNT-DIGITS/32 and COUNT-DIGITS/64 fail when given 0, but that
;;; is immaterial because the 0 value is handled specially in
;;; SIGNIFICAND-DIGITS/32 and SIGNIFICAND-DIGITS/64.
;;;
;;; [1]: https://github.com/lemire/Code-used-on-Daniel-Lemire-s-blog/blob/693681a91167b0b694294bea35f5716c2d2ee264/2021/06/03/digitcount.c
;;; [2]: https://github.com/lemire/Code-used-on-Daniel-Lemire-s-blog/blob/693681a91167b0b694294bea35f5716c2d2ee264/2021/06/03/generate.py
;;; [3]: https://github.com/miloyip/itoa-benchmark/blob/6b66399db63358157892c258a2daa75c07173b05/src/tmueller.cpp
;;; [4]: https://lemire.me/blog/2021/06/03/computing-the-number-of-digits-of-an-integer-even-faster/

;;; Based on https://github.com/lemire/Code-used-on-Daniel-Lemire-s-blog/blob/693681a91167b0b694294bea35f5716c2d2ee264/2021/06/03/generate.py#L9-L17
(defun compute-count-digits/32-table ()
  (loop with result = (make-array 32)
        for i from 1 upto 32
        do (let ((log10 (ceiling (log (ash 1 (1- i)) 10))))
             (setf (aref result (1- i))
                   (if (< i 31)
                       (+ #.(ash 1 32) (- (expt 10 log10)) (ash log10 32))
                       (ash log10 32))))
        finally (return result)))

;;; Based on https://github.com/lemire/Code-used-on-Daniel-Lemire-s-blog/blob/693681a91167b0b694294bea35f5716c2d2ee264/2021/06/03/digitcount.c#L40-L50
(defun count-digits/32 (integer)
  (declare (optimize speed)
           ((unsigned-byte 32) integer))
  (ash (+ integer
          (svref #(4294967295 8589934582 8589934582 8589934582 12884901788 12884901788
                   12884901788 17179868184 17179868184 17179868184 21474826480 21474826480
                   21474826480 21474826480 25769703776 25769703776 25769703776 30063771072
                   30063771072 30063771072 34349738368 34349738368 34349738368 34349738368
                   38554705664 38554705664 38554705664 41949672960 41949672960 41949672960
                   42949672960 42949672960)
                 (1- (integer-length integer))))
       -32))

;;; Based on https://github.com/miloyip/itoa-benchmark/blob/6b66399db63358157892c258a2daa75c07173b05/src/tmueller.cpp#L108-L113
(defun count-digits/64 (integer)
  (declare (optimize speed)
           ((unsigned-byte 64) integer))
  (let ((n (ash (* 1233 (integer-length integer)) -12)))
    (when (>= integer (svref #(1
                               10
                               100
                               1000
                               10000
                               100000
                               1000000
                               10000000
                               100000000
                               1000000000
                               10000000000
                               100000000000
                               1000000000000
                               10000000000000
                               100000000000000
                               1000000000000000
                               10000000000000000
                               100000000000000000
                               1000000000000000000
                               10000000000000000000)
                             n))
      (incf n))
    n))

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

(defun significand-digits/32 (significand exponent sign)
  (declare (optimize speed)
           ((unsigned-byte 32) significand)
           (fixnum exponent))
  (when (zerop significand)
    (return-from significand-digits/32 (values #(0) exponent sign)))
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
          exponent (+ exponent s)))
  ;; Convert significand into digits.
  ;;
  ;; Extracting this into an inline function causes a performance hit
  ;; on SBCL, so leave it here.
  ;;
  ;; The division by 10 could be optimized by reinterpreting the
  ;; significand in fixed point arithmetic with the decimal point to
  ;; the left of the leading digit and multiplying by 10 at each
  ;; iteration instead [1,2].
  ;;
  ;; [1]: https://lemire.me/blog/2021/05/17/converting-binary-integers-to-ascii-characters-apple-m1-vs-amd-zen2/#comment-584345
  ;; [2]: https://stackoverflow.com/questions/7890194/optimized-itoa-function/32818030#32818030
  (loop with digits = (make-array (count-digits/32 significand))
        with digit
        for i from (1- (length digits)) downto 0
        do (multiple-value-setq (significand digit) (floor significand 10))
        do (setf (aref digits i) digit)
        finally (return (values digits exponent sign))))

(defun significand-digits/64 (significand exponent sign)
  (declare (optimize speed)
           ((unsigned-byte 64) significand)
           (fixnum exponent))
  (when (zerop significand)
    (return-from significand-digits/64 (values #(0) exponent sign)))
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
          exponent (+ exponent s)))
  ;; Convert significand into digits.
  (loop with digits = (make-array (count-digits/64 significand))
        with digit
        for i from (1- (length digits)) downto 0
        do (multiple-value-setq (significand digit) (floor significand 10))
        do (setf (aref digits i) digit)
        finally (return (values digits exponent sign))))

;;; Client

(defclass client () ())

(defmethod quaviver:float-decimal :around ((client client) (value single-float))
  (multiple-value-call #'significand-digits/32 (call-next-method)))

(defmethod quaviver:float-decimal :around ((client client) (value double-float))
  (multiple-value-call #'significand-digits/64 (call-next-method)))
