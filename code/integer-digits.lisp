;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; This file also contains code ported from Daniel Lemire's blog [1],
;;;; which he has dedicated to the public domain.
;;;; This file also contains code ported from itoa-benchmark [2], which
;;;; at the time of the port was copyright 2014-2016 Milo Yip and
;;;; licensed under the MIT license (Expat).
;;;;
;;;; Any original code herein is licensed under the MIT license (Expat).
;;;;
;;;; [1]: https://github.com/lemire/Code-used-on-Daniel-Lemire-s-blog/blob/693681a91167b0b694294bea35f5716c2d2ee264/2021/06/03
;;;; [2]: https://github.com/miloyip/itoa-benchmark

(in-package #:quaviver)

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
;;; DIGIT-VECTOR/32 and DIGIT-VECTOR/64.
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

(defun digit-vector/32 (value)
  ;; Convert value into digits.
  ;;
  ;; The division by 10 could be optimized by reinterpreting the
  ;; value in fixed point arithmetic with the decimal point to
  ;; the left of the leading digit and multiplying by 10 at each
  ;; iteration instead [1,2].
  ;;
  ;; [1]: https://lemire.me/blog/2021/05/17/converting-binary-integers-to-ascii-characters-apple-m1-vs-amd-zen2/#comment-584345
  ;; [2]: https://stackoverflow.com/questions/7890194/optimized-itoa-function/32818030#32818030
  (loop with digits = (make-array (count-digits/32 value))
        with digit
        for i from (1- (length digits)) downto 0
        do (multiple-value-setq (value digit) (floor value 10))
           (setf (aref digits i) digit)
        finally (return digits)))

(defun digit-vector/64 (value)
  (declare (optimize speed)
           (type (unsigned-byte 64) value))
  ;; Convert value into digits.
  (loop with digits = (make-array (count-digits/64 value))
        with digit
        for i from (1- (length digits)) downto 0
        finally (return digits)
        do (multiple-value-setq (value digit) (floor value 10))
           (setf (aref digits i) digit)))

(defmethod quaviver:integer-digits
    (client (result-type (eql 'vector)) (base (eql 10)) value)
  (declare (ignore client))
  (if (zerop value)
      #(0)
      (etypecase value
        ((unsigned-byte 32)
         (digits/32 value))
        ((unsigned-byte 64)
         (digits/64 value)))))

(defun digit-string/32 (value)
  ;; Convert value into string of digits.
  ;;
  ;; The division by 10 could be optimized by reinterpreting the
  ;; value in fixed point arithmetic with the decimal point to
  ;; the left of the leading digit and multiplying by 10 at each
  ;; iteration instead [1,2].
  ;;
  ;; [1]: https://lemire.me/blog/2021/05/17/converting-binary-integers-to-ascii-characters-apple-m1-vs-amd-zen2/#comment-584345
  ;; [2]: https://stackoverflow.com/questions/7890194/optimized-itoa-function/32818030#32818030
  (loop with digits = (make-string (count-digits/32 value)
                                  :element-type 'base-char)
        with digit
        for i from (1- (length digits)) downto 0
        do (multiple-value-setq (value digit) (floor value 10))
           (setf (aref digits i) (digit-char digit))
        finally (return digits)))

(defun digit-string/64 (value)
  (declare (optimize speed)
           (type (unsigned-byte 64) value))
  ;; Convert value into digits.
  (loop with digits = (make-string (count-digits/64 value)
                                   :element-type 'base-char)
        with digit
        for i from (1- (length digits)) downto 0
        finally (return digits)
        do (multiple-value-setq (value digit) (floor value 10))
           (setf (aref digits i) (digit-char digit))))

(defmethod quaviver:integer-digits
    (client (result-type (eql 'string)) (base (eql 10)) value)
  (declare (ignore client))
  (if (zerop value)
      #(0)
      (etypecase value
        ((unsigned-byte 32)
         (digit-string/32 value))
        ((unsigned-byte 64)
         (digit-string/64 value)))))

(defmethod quaviver:integer-digits
    (client (result-type (eql 'list)) base value)
  (declare (ignore client))
  (if (zerop value)
      (list 0)
      (prog (digits digit)
       next
         (unless (zerop value)
           (multiple-value-setq (value digit) (floor value base))
           (push digit digits)
           (go next))
         (return digits))))
