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

(in-package #:quaviver/dragonbox)

(defclass client (quaviver/trailing-zeros:client)
  ((%decimal-binary-rounding
    :initarg :decimal-binary-rounding
    :reader decimal-binary-rounding
    :type (member :nearest-to-even
                  :nearest-to-odd
                  :nearest-toward-plus-infinity
                  :nearest-toward-minus-infinity
                  :nearest-away-from-zero
                  :nearest-toward-zero
                  :nearest-to-even-static-boundary
                  :nearest-to-odd-static-boundary
                  :nearest-toward-plus-infinity-static-boundary
                  :nearest-toward-minus-infinity-static-boundary
                  :toward-plus-infinity
                  :toward-minus-infinity
                  :toward-zero
                  :away-from-zero))
   (%binary-decimal-rounding
    :initarg :binary-decimal-rounding
    :reader binary-decimal-rounding
    :type (member :do-not-care
                  :to-even
                  :to-odd
                  :away-from-zero
                  :toward-zero)))
  (:default-initargs
   :decimal-binary-rounding :nearest-to-even
   :binary-decimal-rounding :to-even))

(defmethod initialize-instance :after
    ((client client) &key decimal-binary-rounding binary-decimal-rounding)
  (case decimal-binary-rounding
    ((:nearest-to-even
      :nearest-to-odd
      :nearest-toward-plus-infinity
      :nearest-toward-minus-infinity
      :nearest-away-from-zero
      :nearest-toward-zero
      :nearest-to-even-static-boundary
      :nearest-to-odd-static-boundary
      :nearest-toward-plus-infinity-static-boundary
      :nearest-toward-minus-infinity-static-boundary
      :toward-plus-infinity
      :toward-minus-infinity
      :toward-zero
      :away-from-zero))
    (t
     (error "Decimal to binary rounding mode ~S is unknown." decimal-binary-rounding)))
  (case binary-decimal-rounding
    ((:do-not-care
      :to-even
      :to-odd
      :away-from-zero
      :toward-zero))
    (t
     (error "Binary to decimal rounding mode ~S is unknown." binary-decimal-rounding))))

;;; Decimal to binary rounding
;;;
;;; The intervals are only for COMPUTE-NEAREST, so
;;; :TOWARD-PLUS-INFINITY, :TOWARD-MINUS-INFINITY, :TOWARD-ZERO and
;;; :AWAY-FROM-ZERO do not come into play.

(declaim (inline symmetric-interval))
(defun symmetric-interval (closed-p)
  (values closed-p closed-p))

(declaim (inline asymmetric-interval))
(defun asymmetric-interval (left-closed-p)
  (values left-closed-p (not left-closed-p)))

(declaim (inline closed-interval))
(defun closed-interval ()
  (values t t))

(declaim (inline open-interval))
(defun open-interval ()
  (values nil nil))

(declaim (inline left-closed-right-open-interval))
(defun left-closed-right-open-interval ()
  (values t nil))

(declaim (inline right-closed-left-open-interval))
(defun right-closed-left-open-interval ()
  (values nil t))

(declaim (inline decimal-binary-rounding-normal-interval))
(defun decimal-binary-rounding-normal-interval (rounding significand sign)
  (ecase rounding
    (:nearest-to-even
     (symmetric-interval (evenp significand)))
    (:nearest-to-odd
     (symmetric-interval (oddp significand)))
    (:nearest-toward-plus-infinity
     (asymmetric-interval (plusp sign)))
    (:nearest-toward-minus-infinity
     (asymmetric-interval (minusp sign)))
    (:nearest-away-from-zero
     (left-closed-right-open-interval))
    (:nearest-toward-zero
     (right-closed-left-open-interval))
    (:nearest-to-even-static-boundary
     (if (evenp significand) (closed-interval) (open-interval)))
    (:nearest-to-odd-static-boundary
     (if (oddp significand) (closed-interval) (open-interval)))
    (:nearest-toward-plus-infinity-static-boundary
     (if (plusp sign) (left-closed-right-open-interval) (right-closed-left-open-interval)))
    (:nearest-toward-minus-infinity-static-boundary
     (if (minusp sign) (left-closed-right-open-interval) (right-closed-left-open-interval)))))

(declaim (inline decimal-binary-rounding-shorter-interval))
(defun decimal-binary-rounding-shorter-interval (rounding significand sign)
  (ecase rounding
    (:nearest-to-even
     (closed-interval))
    (:nearest-to-odd
     (open-interval))
    (:nearest-toward-plus-infinity
     (asymmetric-interval (plusp sign)))
    (:nearest-toward-minus-infinity
     (asymmetric-interval (minusp sign)))
    (:nearest-away-from-zero
     (left-closed-right-open-interval))
    (:nearest-toward-zero
     (right-closed-left-open-interval))
    (:nearest-to-even-static-boundary
     (if (evenp significand) (closed-interval) (open-interval)))
    (:nearest-to-odd-static-boundary
     (if (oddp significand) (closed-interval) (open-interval)))
    (:nearest-toward-plus-infinity-static-boundary
     (if (plusp sign) (left-closed-right-open-interval) (right-closed-left-open-interval)))
    (:nearest-toward-minus-infinity-static-boundary
     (if (minusp sign) (left-closed-right-open-interval) (right-closed-left-open-interval)))))

(defun computation-direction (decimal-binary-rounding value)
  (ecase decimal-binary-rounding
    ((:nearest-to-even
      :nearest-to-odd
      :nearest-toward-plus-infinity
      :nearest-toward-minus-infinity
      :nearest-away-from-zero
      :nearest-toward-zero
      :nearest-to-even-static-boundary
      :nearest-to-odd-static-boundary
      :nearest-toward-plus-infinity-static-boundary
      :nearest-toward-minus-infinity-static-boundary)
     :nearest)
    (:toward-plus-infinity
     (if (plusp value) :right-closed-directed :left-closed-directed))
    (:toward-minus-infinity
     (if (minusp value) :right-closed-directed :left-closed-directed))
    (:away-from-zero :right-closed-directed)
    (:toward-zero :left-closed-directed)))

;;; Binary to decimal rounding

(declaim (inline binary-decimal-rounding-prefer-round-down-p))
(defun binary-decimal-rounding-prefer-round-down-p (rounding significand)
  (ecase rounding
    (:do-not-care nil)
    (:to-even (oddp significand))
    (:to-odd (evenp significand))
    (:away-from-zero nil)
    (:toward-zero t)))

;;; Miscellaneous helpers

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun count-factors (number divisor)
    (loop with remainder
          do (multiple-value-setq (number remainder) (floor number divisor))
          while (zerop remainder)
          count 1)))

;;; Logarithms

;;; The body consists of forms of the following form:
;;;
;;;   (supported-lower-limit supported-upper-limit multiply subtract shift)
(defmacro define-log (name &body body)
  (destructuring-bind (default-lower default-upper &rest args)
      (car (last body))
    (declare (ignore args))
    `(defmacro ,name (number &optional (lower-limit ,default-lower)
                               (upper-limit ,default-upper))
       (loop for (supported-lower supported-upper multiply subtract shift)
               in ',body
             do (when (and (>= lower-limit supported-lower)
                           (<= upper-limit supported-upper))
                  (return
                    `(progn
                       #+(or) (assert (<= ,lower-limit ,number ,upper-limit))
                       (ash (- (* ,number ,multiply) ,subtract) ,(- shift)))))
             finally (error "Limits [~A..~A] exceed supported limits [~A..~A]."
                            lower-limit upper-limit supported-lower supported-upper)))))

;;; (floor (log (expt 10 number) 2))
(define-log floor-log2-expt10
  (-15 18 53 0 4)
  (-58 58 1701 0 9)
  (-1233 1233 1741647 0 19))

;;; (floor (log (expt 2 number) 5))
(define-log floor-log5-expt2
  (-1831 1831 225799 0 19))

;;; (floor (- (log (expt 2 number) 5) (log 3 5)))
(define-log floor-log5-expt2-minus-log5-3
  (-3543 2427 451597 715764 20))

;;; (floor (log (expt 2 number) 10))
(define-log floor-log10-expt2
  (-102 102 77 0 8)
  (-425 425 1233 0 12)
  (-2620 2620 315653 0 20))

;;; (floor (- (log (expt 2 number) 10) (log 4/3 10)))
(define-log floor-log10-expt2-minus-log10-4/3
  (-75 129 77 31 8)
  (-424 315 19728 8241 16)
  (-2985 2936 631305 261663 21))

;;; Divisions
;;;
;;; These do not quite speed things up on SBCL.
;;; Consider removing them, but check SBCL's compiler notes before, and
;;; also test on other implementations.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun floor-by-pow10-info (power)
    (ecase power
      ;; (values magic shift)
      (1 (values 6554 16))
      (2 (values 656 16)))))

(defmacro check-divisibility-and-floor-by-pow10 (number power n-bits)
  #+(or) `(multiple-value-bind (result remainder)
              (floor (the (unsigned-byte ,n-bits) ,number) ,(expt 10 power))
            (values result (zerop remainder)))
  (multiple-value-bind (magic position) (floor-by-pow10-info power)
    (let ((prod (gensym (string 'prod))))
      `(progn
         #+(or) (assert (<= ,number ,(expt 10 (1+ power)))) ; slows things down
         (let ((,prod (* (the (unsigned-byte ,n-bits) ,number) ,magic)))
           (values (ldb (byte ,(- n-bits position) ,position) ,prod)
                   (< (ldb (byte ,position 0) ,prod)
                      ,magic)))))))

(defmacro floor-by-pow10-small (number power n-bits)
  #+(or) `(floor (the (unsigned-byte ,n-bits) ,number) ,(expt 10 power))
  (multiple-value-bind (magic position) (floor-by-pow10-info power)
    `(progn
       #+(or) (assert (<= ,number ,(expt 10 (1+ power)))) ; slows things down
       (ldb (byte ,(- n-bits position) ,position)
            (* (the (unsigned-byte ,n-bits) ,number) ,magic)))))

(defmacro floor-by-pow10 (number power n-bits max-power)
  #+(or) `(floor (the (unsigned-byte ,n-bits) ,number) ,(expt 10 power))
  (assert (not (minusp power)))
  (cond ((and (= n-bits 32) (= power 1) (<= max-power 1073741828))
         `(ldb (byte 32 32) (* (the (unsigned-byte ,n-bits) ,number) 429496730)))
        ((and (= n-bits 64) (= power 1) (<= max-power 4611686018427387908))
         `(ldb (byte 64 64) (* (the (unsigned-byte ,n-bits) ,number) 1844674407370955162)))
        ((and (= n-bits 32) (= power 2))
         `(ldb (byte 27 37) (* (the (unsigned-byte ,n-bits) ,number) 1374389535)))
        ((and (= n-bits 64) (= power 3) (<= max-power 15534100272597517998))
         `(ldb (byte 56 72) (* (the (unsigned-byte ,n-bits) ,number) 4722366482869645214)))
        (t
         `(floor (the (unsigned-byte ,n-bits) ,number) ,(expt 10 power)))))

;;; Computations

(defmacro %dragonbox
    (client value total-bits significand-bits min-exponent max-exponent expt10 cache-bits)
  (let* ((carrier-bits total-bits)
         (carrier-type `(unsigned-byte ,carrier-bits))
         (remainder-bits carrier-bits)
         (remainder-type `(unsigned-byte ,remainder-bits))
         (exponent-type '(signed-byte 32))
         (decimal-exponent-type exponent-type)
         (shift-amount-type exponent-type)
         (cache-type `(unsigned-byte ,cache-bits))
         (kappa (1- (floor-log10-expt2 (- carrier-bits significand-bits 2))))
         ;; Oh, this is basically what is computed in the body.
         ;; I can figure it out myself.
         (min-k (min (- (floor-log10-expt2-minus-log10-4/3 (- max-exponent significand-bits)))
                     (- kappa (floor-log10-expt2 (- max-exponent significand-bits)))))
         (max-k (+ (max (- (floor-log10-expt2-minus-log10-4/3 (- min-exponent significand-bits)))
                        (- kappa (floor-log10-expt2 (- min-exponent significand-bits))))
                   ;; For the rescaled subnormals.
                   ;; Not sure if this should be added somewhere in the
                   ;; MAX call — would need to read the Dragonbox paper
                   ;; — but it is correct at least for IEEE-754 binary32
                   ;; and binary64.
                   ;;
                   ;; Also, it might be better to compute this with
                   ;; FLOOT-LOG10-EXPT2, to conform to the body.
                   (ceiling (log (ash 1 (1- significand-bits)) 10))))
         (case-shorter-interval-left-endpoint-lower-threshold 2)
         (case-shorter-interval-left-endpoint-upper-threshold
           (+ 2 (floor (log (/ (expt 10 (1+ (count-factors
                                             (1- (ash 1 (+ 2 significand-bits)))
                                             5)))
                               3)
                            2))))
         (case-shorter-interval-right-endpoint-lower-threshold 0)
         (case-shorter-interval-right-endpoint-upper-threshold
           (+ 2 (floor (log (/ (expt 10 (1+ (count-factors
                                             (1+ (ash 1 (+ 1 significand-bits)))
                                             5)))
                               3)
                            2))))
         (shorter-interval-tie-lower-threshold
           (- (- (floor-log5-expt2-minus-log5-3 (+ significand-bits 4))) 2 significand-bits))
         (shorter-interval-tie-upper-threshold
           (- (- (floor-log5-expt2 (+ significand-bits 2))) 2 significand-bits))
         (big-divisor (expt 10 (1+ kappa)))
         (small-divisor (expt 10 kappa)))
    `(block %dragonbox
       (labels (;; Is it safe to use LDB for these?
                (compute-left-endpoint-for-shorter-interval-case (cache beta)
                  (ash (the ,cache-type (- cache (ash cache ,(- (+ significand-bits 2)))))
                       (- (- ,cache-bits ,significand-bits 1 beta))))
                (compute-right-endpoint-for-shorter-interval-case (cache beta)
                  (ash (the ,cache-type (+ cache (ash cache ,(- (+ significand-bits 1)))))
                       (- (- ,cache-bits ,significand-bits 1 beta))))
                (compute-round-up-for-shorter-interval-case (cache beta)
                  (floor (the ,cache-type
                              (1+ (the ,cache-type
                                       (ash cache (- (- ,cache-bits ,significand-bits 2 beta))))))
                         2))
                (compute-mul (u cache)
                  (declare (,carrier-type u))
                  (let* ((r (* u cache))
                         (integer-part (ldb (byte ,carrier-bits ,cache-bits) r))
                         (integer-p (not (ldb-test (byte ,carrier-bits ,carrier-bits) r))))
                    (values integer-part integer-p)))
                (compute-delta (cache beta) ; ?? in reference, this is casted to remainder_type_, maybe need to LDB
                  (ash cache (- (- ,cache-bits 1 beta))))
                (compute-mul-parity (two-f cache beta)
                  (declare (,carrier-type two-f))
                  ;; (assert (<= 1 beta 32))  ; ?? not always <= in reference implementation
                  (let* ((r (* two-f cache))
                         (parity-p (logbitp (- ,cache-bits beta) r))
                         (integer-p (not (ldb-test (byte ,carrier-bits (- ,carrier-bits beta)) r))))
                    (values parity-p integer-p))))
         (declare (inline
                   compute-left-endpoint-for-shorter-interval-case
                   compute-right-endpoint-for-shorter-interval-case
                   compute-round-up-for-shorter-interval-case
                   compute-mul compute-delta compute-mul-parity))
         (let ((significand 0)
               (binary-exponent 0)
               (sign 0)
               (two-fc 0)
               (decimal-binary-rounding nil)
               (binary-decimal-rounding nil))
           (declare (,carrier-type significand two-fc)
                    ((or ,exponent-type keyword) binary-exponent)
                    ((integer -1 1) sign))
           (multiple-value-setq (significand binary-exponent sign)
             (quaviver:float-integer ,client 2 ,value))
           (when (or (not (numberp binary-exponent))
                     (zerop significand))
             (return-from %dragonbox
               (values significand binary-exponent sign)))
           (setf two-fc (ash significand 1)
                 decimal-binary-rounding (decimal-binary-rounding ,client))
           (ecase (computation-direction decimal-binary-rounding ,value)
             (:nearest
              (setf binary-decimal-rounding (binary-decimal-rounding ,client))
              ;; For the shorter interval case, reference Dragonbox additionally checks
              ;; for a normal number, but FLOAT-INTEGER specialized on base 2 rescales
              ;; the subnormals to look like normals, so we suspect this part of the
              ;; algorithm still applies.
              ;;
              ;; Excluding subnormals would require checking
              ;; (> binary-exponent ,(- min-exponent significand-bits)).
              (when (eql significand ,(ash 1 significand-bits)) ; Shorter interval case
                (multiple-value-bind (include-left-endpoint-p include-right-endpoint-p)
                    (decimal-binary-rounding-shorter-interval
                     decimal-binary-rounding significand sign)
                  (let* ((-k (floor-log10-expt2-minus-log10-4/3
                                 binary-exponent
                                 ,(- min-exponent significand-bits)
                                 ,(- max-exponent significand-bits)))
                         (cache (,expt10 -k))
                         (beta (+ binary-exponent (floor-log2-expt10 (- -k) ,min-k ,max-k)))
                         (xi (compute-left-endpoint-for-shorter-interval-case cache beta))
                         (zi (compute-right-endpoint-for-shorter-interval-case cache beta)))
                    (declare (,decimal-exponent-type -k)
                             (,cache-type cache)
                             (,shift-amount-type beta)
                             (,carrier-type xi zi))
                    (when (and (not include-right-endpoint-p)
                               (<= ,case-shorter-interval-right-endpoint-lower-threshold
                                   binary-exponent
                                   ,case-shorter-interval-right-endpoint-upper-threshold))
                      (decf zi))
                    (when (and (not include-left-endpoint-p)
                               (<= ,case-shorter-interval-left-endpoint-lower-threshold
                                   binary-exponent
                                   ,case-shorter-interval-left-endpoint-upper-threshold))
                      (incf xi))
                    (let ((decimal-significand
                            (floor-by-pow10
                             zi 1 ,carrier-bits
                             ,(* 20 (1+ (floor (1+ (ash 1 (1+ significand-bits))) 3))))))
                      (declare (,carrier-type decimal-significand))
                      (when (>= (* 10 decimal-significand) xi)
                        (return-from %dragonbox
                          (values decimal-significand (1+ -k) sign)))
                      (setf decimal-significand
                            (compute-round-up-for-shorter-interval-case cache beta))
                      (cond ((and (binary-decimal-rounding-prefer-round-down-p
                                   binary-decimal-rounding decimal-significand)
                                  (<= ,shorter-interval-tie-lower-threshold
                                      binary-exponent
                                      ,shorter-interval-tie-upper-threshold))
                             (decf decimal-significand))
                            ((< decimal-significand xi)
                             (incf decimal-significand)))
                      (return-from %dragonbox
                        (values decimal-significand -k sign))))))
              (multiple-value-bind (include-left-endpoint-p include-right-endpoint-p)
                  (decimal-binary-rounding-normal-interval
                   decimal-binary-rounding significand sign)
                (let* ((-k (- (floor-log10-expt2
                                  binary-exponent
                                  ,(- min-exponent significand-bits)
                                  ,(- max-exponent significand-bits))
                              ,kappa))
                       (cache (,expt10 -k))
                       (beta (+ binary-exponent (floor-log2-expt10 (- -k) ,min-k ,max-k)))
                       (deltai (compute-delta cache beta)))
                  (declare (,decimal-exponent-type -k)
                           (,cache-type cache)
                           (,shift-amount-type beta)
                           (,remainder-type deltai))
                  (multiple-value-bind (z-result-integer-part z-result-integer-p)
                      (compute-mul (ash (logior two-fc 1) beta) cache)
                    (declare (,carrier-type z-result-integer-part))
                    ;; Step 2: Try larger divisor; remove trailing zeros if necessary
                    (let* ((decimal-significand
                             (floor-by-pow10
                              z-result-integer-part ,(1+ kappa) ,carrier-bits
                              ,(1- (* big-divisor (ash 1 (1+ significand-bits))))))
                           (r (- z-result-integer-part (* ,big-divisor decimal-significand))))
                      (declare (,carrier-type decimal-significand)
                               (,remainder-type r))
                      (block nil
                        (cond ((< r deltai)
                               (when (zerop (logior r
                                                    (if z-result-integer-p 0 1)
                                                    (if include-right-endpoint-p 1 0)))
                                 (cond ((eq binary-decimal-rounding :do-not-care)
                                        (setf decimal-significand (1- (* 10 decimal-significand)))
                                        (return-from %dragonbox
                                          (values decimal-significand (+ -k ,kappa) sign)))
                                       (t
                                        (decf decimal-significand)
                                        (setf r ,big-divisor)
                                        (return)))))
                              ((> r deltai) (return))
                              (t
                               (multiple-value-bind (x-result-parity-p x-result-integer-p)
                                   (compute-mul-parity (1- two-fc) cache beta)
                                 (when (zerop (logior (if x-result-parity-p 1 0)
                                                      (logand (if x-result-integer-p 1 0)
                                                              (if include-left-endpoint-p 1 0))))
                                   (return)))))
                        (return-from %dragonbox
                          (values decimal-significand (+ -k ,kappa 1) sign)))
                      (setf decimal-significand (* 10 decimal-significand))
                      (cond ((eq binary-decimal-rounding :do-not-care)
                             (cond ((not include-right-endpoint-p)
                                    (multiple-value-bind (r divisible-p)
                                        (check-divisibility-and-floor-by-pow10
                                         r ,kappa ,remainder-bits)
                                      (incf decimal-significand
                                            (if (and divisible-p z-result-integer-p) (1- r) r))))
                                   (t (incf decimal-significand
                                            (floor-by-pow10-small r ,kappa ,remainder-bits)))))
                            (t
                             (let* ((dist (+ (- r (floor deltai 2))
                                             ,(floor small-divisor 2)))
                                    (approx-y-parity-p
                                      (logbitp 0 (logxor dist ,(floor small-divisor 2)))))
                               (declare (,remainder-type dist))
                               (multiple-value-bind (dist divisible-by-small-divisor-p)
                                   (check-divisibility-and-floor-by-pow10
                                    dist ,kappa ,remainder-bits)
                                 (incf decimal-significand dist)
                                 (cond (divisible-by-small-divisor-p
                                        (multiple-value-bind (y-result-parity-p y-result-integer-p)
                                            (compute-mul-parity two-fc cache beta)
                                          (cond ((not (eq y-result-parity-p approx-y-parity-p))
                                                 (decf decimal-significand))
                                                ((and (binary-decimal-rounding-prefer-round-down-p
                                                       binary-decimal-rounding decimal-significand)
                                                      y-result-integer-p)
                                                 (decf decimal-significand))))))))))
                      (values decimal-significand (+ -k ,kappa) sign))))))
             (:left-closed-directed
              (let* ((-k (- (floor-log10-expt2
                                binary-exponent
                                ,(- min-exponent significand-bits)
                                ,(- max-exponent significand-bits))
                            ,kappa))
                     (cache (,expt10 -k))
                     (beta (+ binary-exponent (floor-log2-expt10 (- -k) ,min-k ,max-k)))
                     (deltai (compute-delta cache beta)))
                (declare (,decimal-exponent-type -k)
                         (,cache-type cache)
                         (,shift-amount-type beta)
                         (,remainder-type deltai))
                (multiple-value-bind (x-result-integer-part x-result-integer-p)
                    (compute-mul (ash two-fc beta) cache)
                  (declare (,carrier-type x-result-integer-part))
                  ,@(when (eql total-bits 32)
                      `((when (<= binary-exponent -80)
                          (setf x-result-integer-p nil))))
                  (unless x-result-integer-p
                    (incf x-result-integer-part))
                  ;; Step 2: Try larger divisor; remove trailing zeros if necessary
                  (let* ((decimal-significand
                           (floor-by-pow10
                            x-result-integer-part ,(1+ kappa) ,carrier-bits
                            ,(1- (* big-divisor (ash 1 (1+ significand-bits))))))
                         (r (- x-result-integer-part (* ,big-divisor decimal-significand))))
                    (declare (,carrier-type decimal-significand)
                             (,remainder-type r))
                    (unless (zerop r)
                      (incf decimal-significand)
                      (setf r (- ,big-divisor r)))
                    (block nil
                      (cond ((> r deltai) (return))
                            ((= r deltai)
                             (multiple-value-bind (z-result-parity-p z-result-integer-p)
                                 (compute-mul-parity (+ two-fc 2) cache beta)
                               (when (or z-result-parity-p z-result-integer-p)
                                 (return)))))
                      (return-from %dragonbox
                        (values decimal-significand (+ -k ,kappa 1) sign)))
                    (setf decimal-significand
                          (- (* 10 decimal-significand)
                             (floor-by-pow10-small r ,kappa ,remainder-bits)))
                    (values decimal-significand (+ -k ,kappa) sign)))))
             (:right-closed-directed
              ;; Step 1: Schubfach multiplier calculation
              (let* ((shorter-interval-p
                       ;; Like in the :COMPUTE-NEAREST case above (see its comment for
                       ;; more information), we include the subnormals, unlike the
                       ;; reference Dragonbox implementation.
                       (eql significand ,(ash 1 significand-bits)))
                     (-k (- (floor-log10-expt2
                                (- binary-exponent (if shorter-interval-p 1 0))
                                ,(- min-exponent significand-bits)
                                ,(- max-exponent significand-bits))
                            ,kappa))
                     (cache (,expt10 -k))
                     ;; Note that in the reference implementation, the min-k max-k limits
                     ;; are omitted.
                     (beta (+ binary-exponent (floor-log2-expt10 (- -k) ,min-k ,max-k)))
                     (deltai (compute-delta cache (- beta (if shorter-interval-p 1 0))))
                     (zi (multiple-value-bind (z-result-integer-part z-result-integer-p)
                             (compute-mul (ash two-fc beta) cache)
                           (declare (ignore z-result-integer-p))
                           z-result-integer-part)))
                (declare (,decimal-exponent-type -k)
                         (,cache-type cache)
                         (,shift-amount-type beta)
                         (,remainder-type deltai)
                         (,carrier-type zi))
                ;; Step 2: Try larger divisor; remove trailing zeros if necessary
                (let* ((decimal-significand
                         (floor-by-pow10
                          zi ,(1+ kappa) ,carrier-bits
                          ,(1- (* big-divisor (ash 1 (1+ significand-bits))))))
                       (r (- zi (* ,big-divisor decimal-significand))))
                  (declare (,carrier-type decimal-significand)
                           (,remainder-type r))
                  (block nil
                    (cond ((> r deltai) (return))
                          ((= r deltai)
                           (multiple-value-bind (x-result-parity-p x-result-integer-p)
                               (compute-mul-parity (- two-fc (if shorter-interval-p 1 2))
                                                   cache beta)
                             (declare (ignore x-result-integer-p))
                             (when x-result-parity-p
                               (return)))))
                    (return-from %dragonbox
                      (values decimal-significand (+ -k ,kappa 1) sign)))
                  ;; Step 3: Find the significand with the small divisor
                  (setf decimal-significand
                        (+ (* 10 decimal-significand)
                           (floor-by-pow10-small r ,kappa ,remainder-bits)))
                  (values decimal-significand (+ -k ,kappa) sign))))))))))

(defmethod quaviver:float-integer ((client client) (base (eql 10)) (value single-float))
  ;; (declare (optimize speed))
  (%dragonbox client value 32 23 -126 127 quaviver/math:expt10/32 64))

(defmethod quaviver:float-integer ((client client) (base (eql 10)) (value double-float))
  ;; (declare (optimize speed))
  (%dragonbox client value 64 52 -1022 1023 quaviver/math:expt10/64 128))

#+(or)
(progn
  (asdf:load-system "quaviver/ieee754")

  (defun bits-float32 (x)
    (quaviver:bits-float (make-instance 'quaviver/ieee754:client) 'single-float x))

  (defun bits-float64 (x)
    (quaviver:bits-float (make-instance 'quaviver/ieee754:client) 'double-float x))

  (quaviver:float-integer (make-instance 'client :decimal-binary-rounding :toward-zero)
                          10 3.1232342f2)
  (quaviver:float-integer (make-instance 'client :decimal-binary-rounding :away-from-zero)
                          10 3.1232342f2)
  (quaviver:float-integer (make-instance 'client) 10 1.3423413242f0)
  (quaviver:float-integer (make-instance 'client) 10 #.(expt 2f0 -126))
  (quaviver:float-integer (make-instance 'client) 10 (bits-float32 (dpb -1 (byte 8 23) 0)))
  (quaviver:float-integer (make-instance 'client) 10 (bits-float32 #x800000)) ; exp 1 is -149
  (quaviver:float-integer (make-instance 'client) 10 (bits-float32 #x1000000))
  (quaviver:float-integer (make-instance 'client) 10 (bits-float32 #x000001))

  (quaviver:float-integer nil 2 (bits-float32 #x000001))
  (quaviver:float-integer nil 2 (bits-float32 (ash 1 23)))

  (floor-log10-expt2 22)                ; ?? why does this give 6 instead of 7?
  (floor-log10-expt2 51)                ; and this 15 instead of 16

  (ceiling (log (ash 1 23) 10))
  (ceiling (log (ash 1 22) 10))

  (quaviver:float-integer nil 2 (bits-float64 #x000001))
  (quaviver:float-integer nil 2 (bits-float64 (ash 1 52)))
  (ceiling (log (ash 1 52) 10))
  (ceiling (log (ash 1 51) 10))

  (quaviver:float-integer (make-instance 'quaviver/schubfach:client) 10 (bits-float32 #x000001))
  (quaviver:float-integer (make-instance 'client) 10 (bits-float32 #x7FFFFF))
  (quaviver:float-integer (make-instance 'quaviver/dragonbox:client) 10 (bits-float64 #x000FFFFFFFFFFFFF))
  (quaviver:float-integer (make-instance 'quaviver/schubfach:client) 10 (bits-float64 #x000FFFFFFFFFFFFF))

  nil)

(defun dragonbox-cli (value)
  (nth-value 0 (uiop:run-program (list "/home/paul/polymtl/external/dragonbox/dragonbox-cli" value)
                                 :output '(:string :stripped t)
                                 :error-output *error-output*)))

#+(or)
(progn
  (dragonbox-cli "1.5")
  (dragonbox-cli "1")
  (dragonbox-cli "3.123")
  (dragonbox-cli "3.1232342e2")
  (dragonbox-cli "1.3423413242")
  (dragonbox-cli "1.4012985e-45")
  (dragonbox-cli "1.1754944e-38")
  (dragonbox-cli "9.8607613e-32")       ; FIXME

  nil)

#+(or)
(progn
  (quaviver:float-integer (make-instance 'quaviver/dragonbox:client) 10 9.8607613f-32)
  (quaviver:float-integer (make-instance 'quaviver/dragonbox:client) 10 (bits-float32 1275068420))
  (quaviver:float-integer (make-instance 'quaviver/dragonbox:client) 10 (bits-float32 1275068426))
  (quaviver:float-integer (make-instance 'quaviver/dragonbox:client
                                         :binary-decimal-rounding :nearest-to-odd)
                          10 (bits-float32 1275069386))

  (quaviver:float-integer (make-instance 'quaviver/burger-dybvig:client) 10 9.8607613f-32)
  (quaviver:float-integer (make-instance 'quaviver/burger-dybvig:client) 10 (bits-float32 1275068420))
  (quaviver:float-integer (make-instance 'quaviver/burger-dybvig:client) 10 (bits-float32 1275068426))

  (quaviver/benchmark:float-integer)
  nil)

(defvar *compare-clients/even*
  '(;; :burger-dybvig (quaviver/burger-dybvig:client)
    :schubfach (quaviver/schubfach:client)
    :dragonbox (quaviver/dragonbox:client :decimal-binary-rounding :nearest-to-even)))

(defvar *compare-clients/odd*
  '(;; :burger-dybvig (quaviver/burger-dybvig:client)
    :schubfach (quaviver/schubfach:client)
    :dragonbox (quaviver/dragonbox:client :decimal-binary-rounding :nearest-to-odd)))

#+(or)
;; missing precision compared to Shubfach
(progn
  (201326592 9.8607613e-32 (:SCHUBFACH) (98607613 -39 1) (:DRAGONBOX) (9860761 -38 1))
  (251658240 6.3108872e-30 (:SCHUBFACH) (63108872 -37 1) (:DRAGONBOX) (6310887 -36 1))
  (260046848 1.2621775e-29 (:SCHUBFACH) (12621775 -36 1) (:DRAGONBOX) (12621774 -36 1))
  (318767104 1.6155871e-27 (:SCHUBFACH) (16155871 -34 1) (:DRAGONBOX) (1615587 -33 1))
  (822083584 1.8626451e-9 (:SCHUBFACH) (18626451 -16 1) (:DRAGONBOX) (1862645 -15 1))
  (847249408 1.4901161e-8 (:SCHUBFACH) (14901161 -15 1) (:DRAGONBOX) (1490116 -14 1))
  (956301312 1.2207031e-4 (:SCHUBFACH) (12207031 -11 1) (:DRAGONBOX) (1220703 -10 1))
  (1694498816 3.7778932e22 (:SCHUBFACH) (37778932 15 1) (:DRAGONBOX) (3777893 16 1))
  (1786773504 7.7371252e25 (:SCHUBFACH) (77371252 18 1) (:DRAGONBOX) (7737125 19 1))
  (1795162112 1.5474251e26 (:SCHUBFACH) (15474251 19 1) (:DRAGONBOX) (1547425 20 1))
  (1803550720 3.0948501e26 (:SCHUBFACH) (30948501 19 1) (:DRAGONBOX) (309485 21 1))
  (1811939328 6.1897002e26 (:SCHUBFACH) (61897002 19 1) (:DRAGONBOX) (61897 22 1))
  (1820327936 1.2379401e27 (:SCHUBFACH) (12379401 20 1) (:DRAGONBOX) (123794 22 1))
  (1828716544 2.4758801e27 (:SCHUBFACH) (24758801 20 1) (:DRAGONBOX) (247588 22 1))
  (1837105152 4.9517602e27 (:SCHUBFACH) (49517602 20 1) (:DRAGONBOX) (495176 22 1))
  (1845493760 9.9035203e27 (:SCHUBFACH) (99035203 20 1) (:DRAGONBOX) (990352 22 1))
  (1853882368 1.9807041e28 (:SCHUBFACH) (19807041 21 1) (:DRAGONBOX) (1980704 22 1))
  (1862270976 3.9614081e28 (:SCHUBFACH) (39614081 21 1) (:DRAGONBOX) (3961408 22 1))
  (1870659584 7.9228163e28 (:SCHUBFACH) (79228163 21 1) (:DRAGONBOX) (7922816 22 1)))

#+(or)
(progn
  (quaviver/compare:float-integer *compare-clients* 'double-float 10 0)
  (quaviver/compare:float-integer *compare-clients/even* 'single-float 10 1275068420)
                                        ; => NIL
  ;; Used to give the same value as even, but now something changed.
  (quaviver/compare:float-integer *compare-clients/odd* 'single-float 10 1275068420)
                                        ; => (1275068420 3.355445e7 (:BURGER-DYBVIG) (3355445 1 1) (:DRAGONBOX) (33554448 0 1))


  ;; Shubfach vs Dragonbox
  (quaviver/compare:float-integer *compare-clients/even* 'single-float 10 1219004220)
 ; => (1219004220 345145.88 (:SCHUBFACH) (34514587 -2 1) (:DRAGONBOX) (34514588 -2 1))
  (quaviver/compare:float-integer *compare-clients/odd* 'single-float 10 1219004220)

  (QUAVIVER:FLOAT-INTEGER
   (MAKE-INSTANCE 'CLIENT :DECIMAL-BINARY-ROUNDING :NEAREST-TO-EVEN) 10
   3.355445e7)

  ;; The wrong one.
  (QUAVIVER:FLOAT-INTEGER
   (MAKE-INSTANCE 'CLIENT :DECIMAL-BINARY-ROUNDING :NEAREST-TO-ODD) 10
   3.355445e7)

  (quaviver:float-integer (make-instance 'client) 10
                          (quaviver:bits-float (make-instance 'quaviver/ieee754:client)
                                               'single-float 1275068420))
                                        ; => 3355445, 1, 1
  (quaviver:float-integer (make-instance 'client :decimal-binary-rounding :nearest-to-odd) 10
                          (quaviver:bits-float (make-instance 'quaviver/ieee754:client)
                                               'single-float 1275068420))
 ; => 33554448, 0, 1

  (quaviver/compare:float-integer-random *compare-clients/odd* 'single-float 10 100000)
  (quaviver/compare:float-integer-random *compare-clients/odd* 'double-float 10 100000)

  (quaviver/compare:float-integer-range *compare-clients/odd* 'single-float 10 0 #x800000)

  (quaviver/compare:float-integer-range *compare-clients/odd* 'single-float 10
                                        1249902581 1275074050 #P"/tmp/bla2.lisp-expr")

  (time
   (quaviver/compare:float-integer-range
    *compare-clients/even* 'single-float 10 0 (ash 1 31)
    #P"/tmp/schubfach-dragonbox.lisp-expr" 16))
  (time
   (quaviver/compare:float-integer-range
    *compare-clients* 'single-float 10 0 (ash 1 31)
    #P"/tmp/quaviver-test.lisp-expr" 16))
  ;; On 16 cores.
  ;; Evaluation took:
  ;; 1578.015 seconds of real time
  ;; 17709.389334 seconds of total run time (17422.631899 user, 286.757435 system)
  ;; [ Real times consist of 124.427 seconds GC time, and 1453.588 seconds non-GC time. ]
  ;; [ Run times consist of 124.499 seconds GC time, and 17584.891 seconds non-GC time. ]
  ;; 1122.26% CPU
  ;; 2,677,575,500,315 processor cycles
  ;; 4,939,539,481,584 bytes consed

  ;; On 16 cores, Schubfach vs Dragonbox:
  ;; Evaluation took:
  ;; 596.587 seconds of real time
  ;; 8654.240483 seconds of total run time (8591.439289 user, 62.801194 system)
  ;; [ Real times consist of 30.895 seconds GC time, and 565.692 seconds non-GC time. ]
  ;; [ Run times consist of 30.899 seconds GC time, and 8623.342 seconds non-GC time. ]
  ;; 1450.63% CPU
  ;; 1,012,290,069,203 processor cycles
  ;; 1,263,656,619,040 bytes consed

  nil)
