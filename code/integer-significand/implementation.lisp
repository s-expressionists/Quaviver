;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT AND (Apache-2.0 WITH LLVM-exception OR BSL-1.0)
;;;;
;;;; This file contains code ported from Dragonbox [1], which at the
;;;; time of the port was copyright 2020–2024 Junekey Jeon and licensed
;;;; under Apache-2.0 WITH LLVM-exception OR BSL-1.0.
;;;; This file also contains code ported from Daniel Lemire's blog [1],
;;;; which he has dedicated to the public domain.
;;;;
;;;; Any original code herein is licensed under the MIT license (Expat).
;;;;
;;;; [1]: https://github.com/jk-jeon/dragonbox
;;;; [2]: https://github.com/lemire/Code-used-on-Daniel-Lemire-s-blog/blob/693681a91167b0b694294bea35f5716c2d2ee264/2021/06/03

(in-package #:quaviver/integer-significand)

;;; Counting digits
;;;
;;; The digit-counting algorithms implemented here — basically faster
;;; versions of (CEILING (LOG INTEGER 10)) — are ports of Daniel
;;; Lemire's code [1,2], which is dedicated to the public domain.
;;; An accompanying description is also available [3].
;;;
;;; The algorithms consist of computing the integer base-2 and base-10
;;; logarithms separately and then dividing them.
;;; The base-2 logarithm can be optimized to
;;; (1- (INTEGER-LENGTH INTEGER)) and the base-10 logarithm with lookup
;;; tables.
;;;
;;; There are two algorithms: one with a smaller lookup table and a few
;;; more arithmetic operations, described in the book Hacker's Delight,
;;; and another with a larger lookup table and fewer arithmetic
;;; operations, credited to Kendall Willets.
;;; Daniel Lemire's implementations support only uint32 data, but they
;;; are trivially extended to uint64 data.
;;;
;;; On SBCL the larger lookup table is faster for uint32 data and the
;;; smaller lookup table is faster for uint64 data, because the lookup
;;; table elements are twice the size of the input data, and so the
;;; larger lookup table contains uint128 bignums.
;;;
;;; Both COUNT-DIGITS/32 and COUNT-DIGITS/64 fail when given 0, but that
;;; is immaterial because the 0 value is handled specially in
;;; REMOVE-TRAILING-ZEROS/32 and REMOVE-TRAILING-ZEROS/64.
;;;
;;; [1]: https://github.com/lemire/Code-used-on-Daniel-Lemire-s-blog/blob/693681a91167b0b694294bea35f5716c2d2ee264/2021/06/03/digitcount.c
;;; [2]: https://github.com/lemire/Code-used-on-Daniel-Lemire-s-blog/blob/693681a91167b0b694294bea35f5716c2d2ee264/2021/06/03/generate.py
;;; [3]: https://lemire.me/blog/2021/06/03/computing-the-number-of-digits-of-an-integer-even-faster/

;;; This computes the table inlined in COUNT-DIGITS/32.
;;; Ported from [1].
;;;
;;; [1]: https://github.com/lemire/Code-used-on-Daniel-Lemire-s-blog/blob/693681a91167b0b694294bea35f5716c2d2ee264/2021/06/03/generate.py#L9
(defun compute-count-digits/32-table ()
  (loop with result = (make-array 32)
        for i from 1 upto 32
        do (let ((log10 (ceiling (log (ash 1 (1- i)) 10))))
             (setf (aref result (1- i))
                   (if (< i 31)
                       (+ #.(ash 1 32) (- (expt 10 log10)) (ash log10 32))
                       (ash log10 32))))
        finally (return result)))

;;; This counts the digits in a uint32 using the algorithm with the
;;; larger lookup table.
;;; Ported from [1].
;;;
;;; [1]: https://github.com/lemire/Code-used-on-Daniel-Lemire-s-blog/blob/693681a91167b0b694294bea35f5716c2d2ee264/2021/06/03/digitcount.c#L40
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

;;; This counts the digits in a uint64 using the algorithm with the
;;; smaller lookup table.
;;; Ported from [1] and extended for uint64 data.
;;;
;;; [1]: https://github.com/lemire/Code-used-on-Daniel-Lemire-s-blog/blob/693681a91167b0b694294bea35f5716c2d2ee264/2021/06/03/digitcount.c#L51
(defun count-digits/64 (integer)
  (declare (optimize speed)
           ((unsigned-byte 64) integer))
  (let ((n (ash (* 9 (1- (integer-length integer))) -5)))
    (when (> integer (svref #(9
                              99
                              999
                              9999
                              99999
                              999999
                              9999999
                              99999999
                              999999999
                              9999999999
                              99999999999
                              999999999999
                              9999999999999
                              99999999999999
                              999999999999999
                              9999999999999999
                              99999999999999999
                              999999999999999999)
                            n))
      (incf n))
    (1+ n)))

;;; Integer to digits

;;; Hmm, if I use jeaiii, they assume a larger buffer, so it would be
;;; equivalent to me using a dynamic-extent vector and then subseq.
;;; Then the algorithm can stay the same and I can drop count-digits/X.

;;; https://github.com/jeaiii/itoa/blob/main/include/itoa/jeaiii_to_text.h

#+(or)
(loop with result = (make-array 100)
      for i below 100
      do (setf (aref result i) i)
      finally (return result))

(declaim (inline integer-digits/32))
(defun integer-digits/32 (integer)
  (declare (optimize speed)
           ((unsigned-byte 32) integer))
  )

(defun integer-digits (integer)
  (declare (optimize speed)
           ((unsigned-byte 64) integer))
  (let ((n integer)
        (b (make-array 17 :element-type '(integer 0 9)))
        (f0 0)
        (f2 0)
        (f4 0)
        (f6 0)
        (f8 0))
    (declare (dynamic-extent b))
    (macrolet ((u (f)
                 `,(floor f)))
      (flet (((setf b) (n i)
               (replace b #(0 0 0 1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0 9
                            1 0 1 1 1 2 1 3 1 4 1 5 1 6 1 7 1 8 1 9
                            2 0 2 1 2 2 2 3 2 4 2 5 2 6 2 7 2 8 2 9
                            3 0 3 1 3 2 3 3 3 4 3 5 3 6 3 7 3 8 3 9
                            4 0 4 1 4 2 4 3 4 4 4 5 4 6 4 7 4 8 4 9
                            5 0 5 1 5 2 5 3 5 4 5 5 5 6 5 7 5 8 5 9
                            6 0 6 1 6 2 6 3 6 4 6 5 6 6 6 7 6 8 6 9
                            7 0 7 1 7 2 7 3 7 4 7 5 7 6 7 7 7 8 7 9
                            8 0 8 1 8 2 8 3 8 4 8 5 8 6 8 7 8 8 8 9
                            9 0 9 1 9 2 9 3 9 4 9 5 9 6 9 7 9 8 9 9)
                        :start1 i :start2 (* 2 n) :end1 (+ i 2)))
             (ret (odd i)
               (return-from integer-digits
                 (subseq b (if odd 1 0) i))))
        ;; (declare (inline (setf b) ret))
        (cond ((< n (u 1e2))
               (setf (b 0) n)
               (ret (< n 10) 2))
              ((< n (u 1e6))
               (cond ((< n (u 1e4))
                      (setf f0 (* n #.(1+ (floor (* 10 (ash 1 24)) (floor 1e3))))
                            (b 0) (ash f0 -24)
                            f2 (* 100 (ldb (byte 24 0) f0))
                            (b 2) (ash f2 -24))
                      (ret (< n (u 1e3)) 4))
                     (t
                      (setf f0 (* n #.(1+ (floor (* 10 (ash 1 32)) (floor 1e5))))
                            (b 0) (ash f0 -32)
                            f2 (* 100 (ldb (byte 32 0) f0))
                            (b 2) (ash f2 -32)
                            f4 (* 100 (ldb (byte 32 0) f2))
                            (b 4) (ash f4 -32))
                      (ret (< n (u 1e5)) 6))))
              ((< n (ash 1 32))
               (cond ((< n (u 1e8))
                      (setf f0 (ash (* n #.(1+ (floor (* 10 (ash 1 48)) (floor 1e7)))) -16)
                            (b 0) (ash f0 -32)
                            f2 (* 100 (ldb (byte 32 0) f0))
                            (b 2) (ash f2 -32)
                            f4 (* 100 (ldb (byte 32 0) f2))
                            (b 4) (ash f4 -32)
                            f6 (* 100 (ldb (byte 32 0) f4))
                            (b 6) (ash f6 -32))
                      (ret (< n (u 1e7)) 8))
                     (t
                      (setf f0 (* n #.(1+ (floor (* 10 (ash 1 57)) (floor 1e9))))
                            (b 0) (ash f0 -57)
                            f2 (* 100 (ldb (byte 57 0) f0))
                            (b 2) (ash f2 -57)
                            f4 (* 100 (ldb (byte 57 0) f2))
                            (b 4) (ash f4 -57)
                            f6 (* 100 (ldb (byte 57 0) f4))
                            (b 6) (ash f6 -57)
                            f8 (* 100 (ldb (byte 57 0) f6))
                            (b 8) (ash f8 -57))
                      (ret (< n (u 1e9)) 10)))))
        (let ((i 0)
              (odd nil))
          (multiple-value-bind (u z) (floor n (u 1e8))
            (declare ((unsigned-byte 32) z)
                     ((unsigned-byte 64) u))
            (format t "u ~S z ~S~%" u z)
            (cond ((< u (u 1e2))
                   (setf (b 0) u)
                   (incf i 2))
                  ((< u (u 1e6))
                   (cond ((< u (u 1e4))
                          (setf f0 (* u #.(1+ (floor (* 10 (ash 1 24)) (floor 1e3))))
                                (b 0) (ash f0 -24)
                                f2 (* 100 (ldb (byte 24 0) f0))
                                (b 2) (ash f2 -24))
                          (setf odd (< u (u 1e3)))
                          (incf i 4))
                         (t
                          (setf f0 (* u #.(1+ (floor (* 10 (ash 1 32)) (floor 1e5))))
                                (b 0) (ash f0 -32)
                                f2 (* 100 (ldb (byte 32 0) f0))
                                (b 2) (ash f2 -32)
                                f4 (* 100 (ldb (byte 32 0) f2))
                                (b 4) (ash f4 -32))
                          (setf odd (< u (u 1e5)))
                          (incf i 6))))
                  ;; TODO: Why not in same as below, like above?
                  ((< u (u 1e8))
                   (setf f0 (ash (* u #.(1+ (floor (* 10 (ash 1 48)) (floor 1e7)))) -16)
                         (b 0) (ash f0 -32)
                         f2 (* 100 (ldb (byte 32 0) f0))
                         (b 2) (ash f2 -32)
                         f4 (* 100 (ldb (byte 32 0) f2))
                         (b 4) (ash f4 -32)
                         f6 (* 100 (ldb (byte 32 0) f4))
                         (b 6) (ash f6 -32))
                   (setf odd (< u (u 1e7)))
                   (incf i 8))
                  ((< u (ash 1 32))
                   (cond
                     (t
                      (setf f0 (* u #.(1+ (floor (* 10 (ash 1 57)) (floor 1e9))))
                            (b 0) (ash f0 -57)
                            f2 (* 100 (ldb (byte 57 0) f0))
                            (b 2) (ash f2 -57)
                            f4 (* 100 (ldb (byte 57 0) f2))
                            (b 4) (ash f4 -57)
                            f6 (* 100 (ldb (byte 57 0) f4))
                            (b 6) (ash f6 -57)
                            f8 (* 100 (ldb (byte 57 0) f6))
                            (b 8) (ash f8 -57))
                      (setf odd (< u (u 1e9)))
                      (incf i 10))))
                  (t
                   (multiple-value-bind (u y) (floor u (u 1e8))
                     (cond ((< u (u 1e2))
                            (setf (b 0) u)
                            (incf i 2))
                           (t
                            (setf f0 (* u #.(1+ (floor (* 10 (ash 1 24)) (floor 1e3))))
                                  (b 0) (ash f0 -24)
                                  f2 (* 100 (ldb (byte 24 0) f0))
                                  (b 2) (ash f2 -24))
                            (setf odd (< u (u 1e3)))
                            (incf i 4)))
                     (setf f0 (1+ (ash (* y #.(1+ (floor (* 10 (ash 1 48)) (floor 1e6)))) -16))
                           (b 0) (ash f0 -32)
                           f2 (* 100 (ldb (byte 32 0) f0))
                           (b 2) (ash f2 -32)
                           f4 (* 100 (ldb (byte 32 0) f2))
                           (b 4) (ash f4 -32)
                           f6 (* 100 (ldb (byte 32 0) f4))
                           (b 6) (ash f6 -32))
                     (incf i 8))))
            (setf f0 (1+ (ash (* z #.(1+ (floor (ash 1 48) (floor 1e6)))) -16))
                  (b 0) (ash f0 -32)
                  f2 (* 100 (ldb (byte 32 0) f0))
                  (b 2) (ash f2 -32)
                  f4 (* 100 (ldb (byte 32 0) f2))
                  (b 4) (ash f4 -32)
                  f6 (* 100 (ldb (byte 32 0) f4))
                  (b 6) (ash f6 -32))
            (incf i 8)
            (subseq b (if odd 1 0) i)))))))

#+(or)
(progn
  (integer-digits 13)
  (integer-digits 1340)
  (integer-digits 134)
  (integer-digits 13444)
  (integer-digits 1344432423)
  (integer-digits 10000000000)
  (integer-digits 13444324233)          ; FIXME
  (integer-digits 1344432423323432)

  (1- (ash 1 24))

  (ldb (byte 24 0) -1))

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
  (values
   (block integer-digits/32
     (let ((n significand)
           ;; 10, not 9, because we may have inserted a leading 0.
           (b (make-array 10 :element-type '(integer 0 9)))
           (f0 0)
           (f2 0)
           (f4 0)
           (f6 0)
           (f8 0))
       (declare (dynamic-extent b))
       (macrolet ((u (f)
                    `,(floor f)))
         (flet (((setf b) (n i)
                  (replace b #(0 0 0 1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0 9
                               1 0 1 1 1 2 1 3 1 4 1 5 1 6 1 7 1 8 1 9
                               2 0 2 1 2 2 2 3 2 4 2 5 2 6 2 7 2 8 2 9
                               3 0 3 1 3 2 3 3 3 4 3 5 3 6 3 7 3 8 3 9
                               4 0 4 1 4 2 4 3 4 4 4 5 4 6 4 7 4 8 4 9
                               5 0 5 1 5 2 5 3 5 4 5 5 5 6 5 7 5 8 5 9
                               6 0 6 1 6 2 6 3 6 4 6 5 6 6 6 7 6 8 6 9
                               7 0 7 1 7 2 7 3 7 4 7 5 7 6 7 7 7 8 7 9
                               8 0 8 1 8 2 8 3 8 4 8 5 8 6 8 7 8 8 8 9
                               9 0 9 1 9 2 9 3 9 4 9 5 9 6 9 7 9 8 9 9)
                           :start1 i :start2 (* 2 n) :end1 (+ i 2)))
                (ret (odd i)
                  (return-from integer-digits/32
                    (subseq b (if odd 1 0) i))))
           ;; (declare (inline (setf b) ret))
           (cond ((< n (u 1e2))
                  (setf (b 0) n)
                  (ret (< n 10) 2))
                 ((< n (u 1e6))
                  (cond ((< n (u 1e4))
                         (setf f0 (* n #.(1+ (floor (* 10 (ash 1 24)) (floor 1e3))))
                               (b 0) (ash f0 -24)
                               f2 (* 100 (ldb (byte 24 0) f0))
                               (b 2) (ash f2 -24))
                         (ret (< n (u 1e3)) 4))
                        (t
                         (setf f0 (* n #.(1+ (floor (* 10 (ash 1 32)) (floor 1e5))))
                               (b 0) (ash f0 -32)
                               f2 (* 100 (ldb (byte 32 0) f0))
                               (b 2) (ash f2 -32)
                               f4 (* 100 (ldb (byte 32 0) f2))
                               (b 4) (ash f4 -32))
                         (ret (< n (u 1e5)) 6))))
                 (t
                  (cond ((< n (u 1e8))
                         (setf f0 (ash (* n #.(1+ (floor (* 10 (ash 1 48)) (floor 1e7)))) -16)
                               (b 0) (ash f0 -32)
                               f2 (* 100 (ldb (byte 32 0) f0))
                               (b 2) (ash f2 -32)
                               f4 (* 100 (ldb (byte 32 0) f2))
                               (b 4) (ash f4 -32)
                               f6 (* 100 (ldb (byte 32 0) f4))
                               (b 6) (ash f6 -32))
                         (ret (< n (u 1e7)) 8))
                        (t
                         (setf f0 (* n #.(1+ (floor (* 10 (ash 1 57)) (floor 1e9))))
                               (b 0) (ash f0 -57)
                               f2 (* 100 (ldb (byte 57 0) f0))
                               (b 2) (ash f2 -57)
                               f4 (* 100 (ldb (byte 57 0) f2))
                               (b 4) (ash f4 -57)
                               f6 (* 100 (ldb (byte 57 0) f4))
                               (b 6) (ash f6 -57)
                               f8 (* 100 (ldb (byte 57 0) f6))
                               (b 8) (ash f8 -57))
                         (ret (< n (u 1e9)) 10)))))))))
   exponent sign)
  #+(or)
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
