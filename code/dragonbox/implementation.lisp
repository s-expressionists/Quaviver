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

;;; Logarithms

;;; Grammar:
;;;
;;;   (define-log NAME
;;;     (SUPPORTED-LOWER-LIMIT SUPPORTED-UPPER-LIMIT MULTIPLY SUBTRACT SHIFT)
;;;     ...)
;;;
;;; The resulting macro's default lower and upper limits are those of
;;; the last body clause.
;;;
;;; TODO: Maybe LDB the result.
(defmacro define-log (name &body body)
  (destructuring-bind (default-lower default-upper &rest args) (car (last body))
    (declare (ignore args))
    `(defmacro ,name (number &optional (lower-limit ,default-lower)
                               (upper-limit ,default-upper))
       (loop for (supported-lower supported-upper multiply subtract shift) in ',body
             do (when (and (>= lower-limit supported-lower)
                           (<= upper-limit supported-upper))
                  (return
                    `(progn
                       ;; (assert (<= ,lower-limit ,number ,upper-limit))
                       (ash (- (* ,number ,multiply) ,subtract) ,(- shift)))))
             finally (error "Limits [~A..~A] lie outside supported limits [~A..~A]."
                            lower-limit upper-limit supported-lower supported-upper)))))

;;; Constants from https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L906-L1146

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
;;; Testing on SBCL at some point seemed to indicate that these macros
;;; only negligibly helped performance.
;;; Consider removing them after testing on more implementations.
;;;
;;; These all compute
;;;
;;;   (floor number (expt 10 power))
;;;
;;; with FLOOR-BY-EXPT10-DIVISIBLE-P additionally checking if the
;;; remainder is 0.
;;;
;;; Based on https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L1152-L1261

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun floor-by-expt10-constants (power)
    (ecase power
      ;; (values MAGIC SHIFT)
      (1 (values 6554 16))
      (2 (values 656 16)))))

(defmacro floor-by-expt10-divisible-p (number power size)
  (assert (<= (1+ power) (floor-log10-expt2 size)))
  (multiple-value-bind (magic position) (floor-by-expt10-constants power)
    (let ((prod (gensym (string 'prod))))
      `(progn
         ;; (assert (<= ,number ,(expt 10 (1+ power))))
         (let ((,prod (* (the (unsigned-byte ,size) ,number) ,magic)))
           (declare ((unsigned-byte 32) ,prod)) ; depends on the constants
           (values (ldb (byte ,(- size position) ,position) ,prod)
                   (< (ldb (byte ,position 0) ,prod)
                      ,magic)))))))

(defmacro floor-by-expt10-small (number power size)
  (assert (<= (1+ power) (floor-log10-expt2 size)))
  (multiple-value-bind (magic position) (floor-by-expt10-constants power)
    `(progn
       ;; (assert (<= ,number ,(expt 10 (1+ power))))
       (ldb (byte ,(- size position) ,position)
            (* (the (unsigned-byte ,size) ,number) ,magic)))))

(defmacro floor-by-expt10 (number power size max-number)
  (assert (not (minusp power)))
  (cond ((and (= size 32) (= power 1) (<= max-number 1073741828))
         `(ldb (byte 32 32) (* (the (unsigned-byte ,size) ,number) 429496730)))
        ((and (= size 64) (= power 1) (<= max-number 4611686018427387908))
         `(ldb (byte 64 64) (* (the (unsigned-byte ,size) ,number) 1844674407370955162)))
        ((and (= size 32) (= power 2))
         `(ldb (byte 27 37) (* (the (unsigned-byte ,size) ,number) 1374389535)))
        ((and (= size 64) (= power 3) (<= max-number 15534100272597517998))
         `(ldb (byte 56 72) (* (the (unsigned-byte ,size) ,number) 4722366482869645214)))
        (t
         `(floor (the (unsigned-byte ,size) ,number) ,(expt 10 power)))))

;;; Protocol

(defgeneric decimal-binary-rounding (client))
(defgeneric binary-decimal-rounding (client))
(defgeneric normal-interval (client significand sign))
(defgeneric shorter-interval (client significand sign))
(defgeneric prefer-round-down-p (client significand))
(defgeneric direction (client value))

;;; Nearest client

(defclass nearest-client (quaviver/trailing-zeros:client)
  ((%decimal-binary-rounding
    :initarg :decimal-binary-rounding
    :reader decimal-binary-rounding
    :type (member :to-even
                  :to-odd
                  :toward-plus-infinity
                  :toward-minus-infinity
                  :away-from-zero
                  :toward-zero
                  :to-even-static-boundary
                  :to-odd-static-boundary
                  :toward-plus-infinity-static-boundary
                  :toward-minus-infinity-static-boundary))
   (%binary-decimal-rounding
    :initarg :binary-decimal-rounding
    :reader binary-decimal-rounding
    :type (member :do-not-care
                  :to-even
                  :to-odd
                  :away-from-zero
                  :toward-zero)))
  (:default-initargs
   :decimal-binary-rounding :to-even
   :binary-decimal-rounding :away-from-zero))

(defmethod initialize-instance :after
    ((client nearest-client) &key decimal-binary-rounding binary-decimal-rounding)
  (case decimal-binary-rounding
    ((:to-even
      :to-odd
      :toward-plus-infinity
      :toward-minus-infinity
      :away-from-zero
      :toward-zero
      :to-even-static-boundary
      :to-odd-static-boundary
      :toward-plus-infinity-static-boundary
      :toward-minus-infinity-static-boundary))
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

;;; Intervals based on https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L2439-L2708

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

(defmethod normal-interval ((client nearest-client) significand sign) ; base 2 significand
  (ecase (decimal-binary-rounding client)
    (:to-even
     (symmetric-interval (evenp significand)))
    (:to-odd
     (symmetric-interval (oddp significand)))
    (:toward-plus-infinity
     (asymmetric-interval (plusp sign)))
    (:toward-minus-infinity
     (asymmetric-interval (minusp sign)))
    (:away-from-zero
     (left-closed-right-open-interval))
    (:toward-zero
     (right-closed-left-open-interval))
    (:to-even-static-boundary
     (if (evenp significand) (closed-interval) (open-interval)))
    (:to-odd-static-boundary
     (if (oddp significand) (closed-interval) (open-interval)))
    (:toward-plus-infinity-static-boundary
     (if (plusp sign) (left-closed-right-open-interval) (right-closed-left-open-interval)))
    (:toward-minus-infinity-static-boundary
     (if (minusp sign) (left-closed-right-open-interval) (right-closed-left-open-interval)))))

(defmethod shorter-interval ((client nearest-client) significand sign) ; base 2 significand
  (ecase (decimal-binary-rounding client)
    (:to-even
     (closed-interval))
    (:to-odd
     (open-interval))
    (:toward-plus-infinity
     (asymmetric-interval (plusp sign)))
    (:toward-minus-infinity
     (asymmetric-interval (minusp sign)))
    (:away-from-zero
     (left-closed-right-open-interval))
    (:toward-zero
     (right-closed-left-open-interval))
    (:to-even-static-boundary
     (if (evenp significand) (closed-interval) (open-interval)))
    (:to-odd-static-boundary
     (if (oddp significand) (closed-interval) (open-interval)))
    (:toward-plus-infinity-static-boundary
     (if (plusp sign) (left-closed-right-open-interval) (right-closed-left-open-interval)))
    (:toward-minus-infinity-static-boundary
     (if (minusp sign) (left-closed-right-open-interval) (right-closed-left-open-interval)))))

;;; Based on https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L2766-L2822
(defmethod prefer-round-down-p ((client nearest-client) significand) ; base 10 significand
  (ecase (binary-decimal-rounding client)
    (:do-not-care nil)
    (:to-even (oddp significand))
    (:to-odd (evenp significand))
    (:away-from-zero nil)
    (:toward-zero t)))

;;; Directed client

(defclass directed-client (quaviver/trailing-zeros:client)
  ((%decimal-binary-rounding
    :initarg :decimal-binary-rounding
    :reader decimal-binary-rounding
    :type (member :toward-plus-infinity
                  :toward-minus-infinity
                  :away-from-zero
                  :toward-zero)))
  (:default-initargs
   :decimal-binary-rounding :away-from-zero))

(defmethod initialize-instance :after
    ((client directed-client) &key decimal-binary-rounding)
  (case decimal-binary-rounding
    ((:toward-plus-infinity
      :toward-minus-infinity
      :away-from-zero
      :toward-zero))
    (t
     (error "Decimal to binary rounding mode ~S is unknown." decimal-binary-rounding))))

;;; Based on https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L2710-L2763
(defmethod direction ((client directed-client) value)
  (ecase (decimal-binary-rounding client)
    (:toward-plus-infinity
     (if (plusp value) :right-closed-directed :left-closed-directed))
    (:toward-minus-infinity
     (if (minusp value) :right-closed-directed :left-closed-directed))
    (:away-from-zero :right-closed-directed)
    (:toward-zero :left-closed-directed)))

;;; Computations

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric kappa (type))
  (defgeneric min-k (type))
  (defgeneric max-k (type))

  ;; Based on https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L3189-L3193
  (defmethod kappa (type)
    (let ((kappa (1- (floor-log10-expt2 (- (quaviver:arithmetic-size type)
                                           (quaviver:significand-size type)
                                           1)))))
      (assert (>= kappa 1))
      (assert (>= (quaviver:arithmetic-size type)
                  (+ (quaviver:significand-size type)
                     1
                     (floor-log2-expt10 (1+ kappa)))))
      kappa))

  ;; MIN-K and MAX-K, based on [1], can be derived by inspecting the
  ;; extreme values that -K can take within %NEAREST and %DIRECTED.
  ;; The resulting values (actually, only MAX-K) are different from
  ;; reference Dragonbox because Quaviver takes into account the
  ;; adjusted subnormals.
  ;;
  ;; [1]: https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L3198-L3207

  (defmethod min-k (type)
    (let ((max-exponent (quaviver:max-exponent type)))
      (min (- (floor-log10-expt2-minus-log10-4/3 max-exponent))
           (- (kappa type) (floor-log10-expt2 max-exponent)))))

  (defmethod max-k (type)
    (let ((min-exponent (quaviver:min-exponent type)))
      (max (- (floor-log10-expt2-minus-log10-4/3 min-exponent))
           (- (kappa type) (floor-log10-expt2 min-exponent)))))

  (defun count-factors (number divisor)
    (assert (> number 0))
    (assert (> divisor 1))
    (loop with remainder
          do (multiple-value-setq (number remainder) (floor number divisor))
          while (zerop remainder)
          count 1)))

;;; Based on
;;; https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L3064-L3068
;;; and
;;; https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L3121-L3125
(defmacro compute-mul (u expt10 arithmetic-size expt10-size)
  (let ((ug (gensym (string 'u)))
        (expt10g (gensym (string 'expt10))))
    `(let* ((,ug ,u)
            (,expt10g ,expt10)
            (r (* ,ug ,expt10g)))
       (declare ((unsigned-byte ,arithmetic-size) ,ug)
                ((unsigned-byte ,expt10-size) ,expt10g))
       (values (ldb (byte ,arithmetic-size ,expt10-size) r) ; integer part
               (zerop (ldb (byte ,arithmetic-size ,arithmetic-size) r)))))) ; integer-p

;;; Based on
;;; https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L3076-L3085
;;; and
;;; https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L3134-L3144
(defmacro compute-mul-parity (u expt10 beta arithmetic-size expt10-size)
  (let ((ug (gensym (string 'u)))
        (expt10g (gensym (string 'expt10)))
        (betag (gensym (string 'beta))))
    `(let* ((,ug ,u)
            (,expt10g ,expt10)
            (,betag ,beta)
            (r (* ,ug ,expt10g)))
       (declare ((unsigned-byte ,arithmetic-size) ,ug)
                ((unsigned-byte ,expt10-size) ,expt10g)
                ,(ecase arithmetic-size
                   (32 `((integer 1 32) ,betag))     ; inclusive
                   (64 `((integer 1 (64)) ,betag)))) ; exclusive
       (values (logbitp (- ,expt10-size ,betag) r)   ; parity-p
               (zerop (ldb (byte ,arithmetic-size (- ,arithmetic-size ,betag)) r)))))) ; integer-p

;;; Based on https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L3247-L3551
(defmacro %nearest (client value type expt10)
  (with-accessors ((arithmetic-size quaviver:arithmetic-size)
                   (significand-size quaviver:significand-size)
                   (min-exponent quaviver:min-exponent)
                   (max-exponent quaviver:max-exponent)
                   (kappa kappa)
                   (min-k min-k)
                   (max-k max-k))
      type
    (let ((expt10-size (ash arithmetic-size 1)))
      `(block %dragonbox
         (let ((significand 0)
               (exponent 0)
               (sign 0)
               (2fc 0))
           (declare ((unsigned-byte ,arithmetic-size) significand 2fc)
                    ((or (signed-byte 32) keyword) exponent)
                    ((integer -1 1) sign))
           (multiple-value-setq (significand exponent sign)
             (quaviver:float-integer ,client 2 ,value))
           (when (or (not (numberp exponent))
                     (zerop significand))
             (return-from %dragonbox
               (values significand exponent sign)))
           (setf 2fc (ash significand 1))
           ;; Shorter interval case
           ;;
           ;; Reference Dragonbox additionally checks for a normal
           ;; number, but FLOAT-INTEGER specialized on base 2 rescales
           ;; the subnormals to look like normals, so this part of the
           ;; algorithm may still apply.
           ;; Exhaustive single-float testing against Burger-Dybvig
           ;; confirms that it is correct for binary32, anyway.
           ;;
           ;; TODO: Exhaustively test the shorter interval case on
           ;; subnormals (only (1- SIGNIFICAND-SIZE) cases to check).
           (when (eql significand ,(ash 1 (1- significand-size)))
             (multiple-value-bind (include-left-endpoint-p include-right-endpoint-p)
                 (shorter-interval ,client significand sign)
               (let* ((-k (floor-log10-expt2-minus-log10-4/3 exponent ,min-exponent ,max-exponent))
                      (beta (+ exponent (floor-log2-expt10 (- -k) ,min-k ,max-k)))
                      (expt10 (,expt10 -k))
                      (xi (ldb (byte ,arithmetic-size 0)
                               (ash (the (unsigned-byte ,expt10-size)
                                         (- expt10 (ash expt10 ,(- (1+ significand-size)))))
                                    (- (- ,expt10-size ,significand-size beta)))))
                      (zi (ldb (byte ,arithmetic-size 0)
                               (ash (the (unsigned-byte ,expt10-size)
                                         (+ expt10 (ash expt10 ,(- significand-size))))
                                    (- (- ,expt10-size ,significand-size beta))))))
                 (declare ((signed-byte 32) -k beta)
                          ((unsigned-byte ,expt10-size) expt10)
                          ((unsigned-byte ,arithmetic-size) xi zi))
                 (when (and (not include-right-endpoint-p)
                            (<= 0 exponent
                                ,(+ 2 (floor (log (/ (expt 10 (1+ (count-factors
                                                                   (1+ (ash 1 significand-size))
                                                                   5)))
                                                     3)
                                                  2)))))
                   (decf zi))
                 (when (or (not include-left-endpoint-p)
                           (not (<= 2 exponent
                                    ,(+ 2 (floor (log (/ (expt 10 (1+ (count-factors
                                                                       (1- (ash 1 (1+ significand-size)))
                                                                       5)))
                                                         3)
                                                      2))))))
                   (incf xi))
                 (setf significand (floor-by-expt10
                                    zi 1 ,arithmetic-size
                                    ,(* 20 (1+ (floor (1+ (ash 1 significand-size)) 3)))))
                 (when (>= (the (unsigned-byte ,arithmetic-size) (* 10 significand)) xi)
                   (return-from %dragonbox
                     (values significand (1+ -k) sign)))
                 (setf significand
                       (ash (the (unsigned-byte ,arithmetic-size)
                                 (1+ (ldb (byte ,arithmetic-size 0)
                                          (ash expt10 (- (- ,expt10-size ,significand-size 1 beta))))))
                            -1))
                 (cond ((and (prefer-round-down-p ,client significand)
                             (<= ,(- (- (floor-log5-expt2-minus-log5-3 (+ significand-size 3)))
                                     2 (1- significand-size))
                                 exponent
                                 ,(- (- (floor-log5-expt2 (1+ significand-size)))
                                     2 (1- significand-size))))
                        (decf significand))
                       ((< significand xi)
                        (incf significand)))
                 (return-from %dragonbox
                   (values significand -k sign)))))
           ;; Step 1: Schubfach multiplier calculation
           (multiple-value-bind (include-left-endpoint-p include-right-endpoint-p)
               (normal-interval ,client significand sign)
             (let* ((-k (- (floor-log10-expt2 exponent ,min-exponent ,max-exponent)
                           ,kappa))
                    (beta (+ exponent (floor-log2-expt10 (- -k) ,min-k ,max-k)))
                    (expt10 (,expt10 -k))
                    (deltai (ldb (byte ,arithmetic-size 0)
                                 (ash expt10 (- (- ,expt10-size 1 beta)))))
                    (zi 0)
                    (zi-integer-p nil)
                    (r 0))
               (declare ((signed-byte 32) -k beta)
                        ((unsigned-byte ,expt10-size) expt10)
                        ((unsigned-byte ,arithmetic-size) deltai zi r)
                        (boolean zi-integer-p))
               (multiple-value-setq (zi zi-integer-p)
                 (compute-mul (ash (logior 2fc 1) beta) expt10 ,arithmetic-size ,expt10-size))
               ;; Step 2: Try larger divisor; remove trailing zeros if necessary
               (setf significand (floor-by-expt10 ; base 10 significand from here on out
                                  zi ,(1+ kappa) ,arithmetic-size
                                  ,(1- (* (expt 10 (1+ kappa)) (ash 1 significand-size))))
                     r (- zi (* ,(expt 10 (1+ kappa)) significand)))
               (block nil
                 (cond ((< r deltai)
                        (when (zerop (logior r
                                             (if zi-integer-p 0 1)
                                             (if include-right-endpoint-p 1 0)))
                          (cond ((eq (binary-decimal-rounding ,client) :do-not-care)
                                 (setf significand (1- (* 10 significand)))
                                 (return-from %dragonbox
                                   (values significand (+ -k ,kappa) sign)))
                                (t
                                 (decf significand)
                                 (setf r ,(expt 10 (1+ kappa)))
                                 (return)))))
                       ((> r deltai) (return))
                       (t
                        (multiple-value-bind (xi-parity-p xi-integer-p)
                            (compute-mul-parity (1- 2fc) expt10 beta ,arithmetic-size ,expt10-size)
                          (when (zerop (logior (if xi-parity-p 1 0)
                                               (logand (if xi-integer-p 1 0)
                                                       (if include-left-endpoint-p 1 0))))
                            (return)))))
                 (return-from %dragonbox
                   (values significand (+ -k ,kappa 1) sign)))
               ;; Step 3: Find the significand with the smaller divisor
               (setf significand (* 10 significand))
               (cond ((eq (binary-decimal-rounding ,client) :do-not-care)
                      (cond ((not include-right-endpoint-p)
                             (multiple-value-bind (r divisible-p)
                                 (floor-by-expt10-divisible-p r ,kappa ,arithmetic-size)
                               (incf significand (if (and divisible-p zi-integer-p) (1- r) r))))
                            (t (incf significand (floor-by-expt10-small r ,kappa ,arithmetic-size)))))
                     (t
                      (let* ((dist (+ (the (unsigned-byte ,arithmetic-size) (- r (ash deltai -1)))
                                      ,(floor (expt 10 kappa) 2)))
                             (approx-y-parity-p
                               (logbitp 0 (logxor dist ,(floor (expt 10 kappa) 2)))))
                        (declare ((unsigned-byte ,arithmetic-size) dist))
                        (multiple-value-bind (dist divisible-p)
                            (floor-by-expt10-divisible-p dist ,kappa ,arithmetic-size)
                          (incf significand dist)
                          (when divisible-p
                            (multiple-value-bind (yi-parity-p yi-integer-p)
                                (compute-mul-parity 2fc expt10 beta ,arithmetic-size ,expt10-size)
                              (cond ((not (eq yi-parity-p approx-y-parity-p))
                                     (decf significand))
                                    ((and (prefer-round-down-p ,client significand)
                                          yi-integer-p)
                                     (decf significand)))))))))
               (values significand (+ -k ,kappa) sign))))))))

;;; Based on https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L3553-L3799
(defmacro %directed (client value type expt10)
  (with-accessors ((arithmetic-size quaviver:arithmetic-size)
                   (significand-size quaviver:significand-size)
                   (min-exponent quaviver:min-exponent)
                   (max-exponent quaviver:max-exponent)
                   (kappa kappa)
                   (min-k min-k)
                   (max-k max-k))
      type
    (let ((expt10-size (ash arithmetic-size 1)))
      `(block %dragonbox
         (let ((significand 0)
               (exponent 0)
               (sign 0)
               (2fc 0))
           (declare ((unsigned-byte ,arithmetic-size) significand 2fc)
                    ((or (signed-byte 32) keyword) exponent)
                    ((integer -1 1) sign))
           (multiple-value-setq (significand exponent sign)
             (quaviver:float-integer ,client 2 ,value))
           (when (or (not (numberp exponent))
                     (zerop significand))
             (return-from %dragonbox
               (values significand exponent sign)))
           (setf 2fc (ash significand 1))
           (ecase (direction ,client ,value)
             (:left-closed-directed
              ;; Step 1: Schubfach multiplier calculation
              (let* ((-k (- (floor-log10-expt2 exponent ,min-exponent ,max-exponent)
                            ,kappa))
                     (beta (+ exponent (floor-log2-expt10 (- -k) ,min-k ,max-k)))
                     (expt10 (,expt10 -k))
                     (deltai (ldb (byte ,arithmetic-size 0)
                                  (ash expt10 (- (- ,expt10-size 1 beta)))))
                     (xi 0)
                     (xi-integer-p nil)
                     (r 0))
                (declare ((signed-byte 32) -k beta)
                         ((unsigned-byte ,expt10-size) expt10)
                         ((unsigned-byte ,arithmetic-size) deltai xi r)
                         (boolean xi-integer-p))
                (multiple-value-setq (xi xi-integer-p)
                  (compute-mul (ash 2fc beta) expt10 ,arithmetic-size ,expt10-size))
                ,@(when (eq type 'single-float)
                    `((when (<= exponent -80)
                        (setf xi-integer-p nil))))
                (unless xi-integer-p
                  (incf xi))
                ;; Step 2: Try larger divisor; remove trailing zeros if necessary
                (setf significand (floor-by-expt10 ; base 10 significand from here on out
                                   xi ,(1+ kappa) ,arithmetic-size
                                   ,(1- (* (expt 10 (1+ kappa)) (ash 1 significand-size))))
                      r (- xi (* ,(expt 10 (1+ kappa)) significand)))
                (unless (zerop r)
                  (incf significand)
                  (setf r (- ,(expt 10 (1+ kappa)) r)))
                (block nil
                  (cond ((> r deltai) (return))
                        ((= r deltai)
                         (multiple-value-bind (zi-parity-p zi-integer-p)
                             (compute-mul-parity (+ 2fc 2) expt10 beta
                                                 ,arithmetic-size ,expt10-size)
                           (when (or zi-parity-p zi-integer-p)
                             (return)))))
                  (return-from %dragonbox
                    (values significand (+ -k ,kappa 1) sign)))
                ;; Step 3: Find the significand with the smaller divisor
                (setf significand (- (* 10 significand)
                                     (floor-by-expt10-small r ,kappa ,arithmetic-size)))
                (values significand (+ -k ,kappa) sign)))
             (:right-closed-directed
              ;; Step 1: Schubfach multiplier calculation
              (let* ((shorter-interval-p
                       ;; Like in %NEAREST (see its comment for more information),
                       ;; we include the subnormals, unlike reference Dragonbox.
                       ;;
                       ;; However, reference Dragonbox additionally checks that the
                       ;; exponent bits are not 1 [1], which is not done here, so
                       ;; something is likely missing here.
                       ;; TODO: Investigate.
                       ;;
                       ;; [1]: https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L3719
                       (eql significand ,(ash 1 (1- significand-size))))
                     (-k (- (floor-log10-expt2 (- exponent (if shorter-interval-p 1 0))
                                               ,min-exponent ,max-exponent)
                            ,kappa))
                     (beta (+ exponent (floor-log2-expt10 (- -k) ,min-k ,max-k)))
                     (expt10 (,expt10 -k))
                     (deltai (ldb (byte ,arithmetic-size 0)
                                  (ash expt10 (- (- ,expt10-size (if shorter-interval-p 0 1) beta)))))
                     (zi (nth-value 0 (compute-mul (ash 2fc beta) expt10
                                                   ,arithmetic-size ,expt10-size)))
                     (r 0))
                (declare ((signed-byte 32) -k beta)
                         ((unsigned-byte ,expt10-size) expt10)
                         ((unsigned-byte ,arithmetic-size) deltai zi r))
                ;; Step 2: Try larger divisor; remove trailing zeros if necessary
                (setf significand (floor-by-expt10 ; base 10 significand from here on out
                                   zi ,(1+ kappa) ,arithmetic-size
                                   ,(1- (* (expt 10 (1+ kappa)) (ash 1 significand-size))))
                      r (- zi (* ,(expt 10 (1+ kappa)) significand)))
                (block nil
                  (cond ((> r deltai) (return))
                        ((= r deltai)
                         (multiple-value-bind (xi-parity-p xi-integer-p)
                             (compute-mul-parity (- 2fc (if shorter-interval-p 1 2))
                                                 expt10 beta ,arithmetic-size ,expt10-size)
                           (declare (ignore xi-integer-p))
                           (unless xi-parity-p
                             (return)))))
                  (return-from %dragonbox
                    (values significand (+ -k ,kappa 1) sign)))
                ;; Step 3: Find the significand with the smaller divisor
                (setf significand (+ (* 10 significand)
                                     (floor-by-expt10-small r ,kappa ,arithmetic-size)))
                (values significand (+ -k ,kappa) sign)))))))))

(defmethod quaviver:float-integer ((client nearest-client) (base (eql 10)) (value single-float))
  (declare (optimize speed))
  (%nearest client value single-float quaviver/math:expt10/32))

(defmethod quaviver:float-integer ((client nearest-client) (base (eql 10)) (value double-float))
  (declare (optimize speed))
  (%nearest client value double-float quaviver/math:expt10/64))

(defmethod quaviver:float-integer ((client directed-client) (base (eql 10)) (value single-float))
  (declare (optimize speed))
  (%directed client value single-float quaviver/math:expt10/32))

(defmethod quaviver:float-integer ((client directed-client) (base (eql 10)) (value double-float))
  (declare (optimize speed))
  (%directed client value double-float quaviver/math:expt10/64))
