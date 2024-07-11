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
      (2 (values 656 16))
      (3 (values 66 16)))))

(defmacro floor-by-expt10-divisible-p (number power size)
  (assert (<= (1+ power) (floor-log10-expt2 size)))
  (multiple-value-bind (magic position) (floor-by-expt10-constants power)
    (let ((prod (gensym (string 'prod))))
      `(progn
         ;; (assert (<= ,number ,(expt 10 (1+ power))))
         (let ((,prod (* (the (quaviver/math:arithmetic-word ,size) ,number) ,magic)))
           (declare ((quaviver/math:arithmetic-word 32) ,prod)) ; depends on the constants
           (values (ldb (byte ,(- size position) ,position) ,prod)
                   (< (ldb (byte ,position 0) ,prod)
                      ,magic)))))))

(defmacro floor-by-expt10-small (number power size)
  (assert (<= (1+ power) (floor-log10-expt2 size)))
  (multiple-value-bind (magic position) (floor-by-expt10-constants power)
    `(progn
       ;; (assert (<= ,number ,(expt 10 (1+ power))))
       (ldb (byte ,(- size position) ,position)
            (* (the (quaviver/math:arithmetic-word ,size) ,number) ,magic)))))

(defmacro floor-by-expt10 (number power size max-number)
  (assert (not (minusp power)))
  (let ((number `(the (quaviver/math:arithmetic-word ,size) ,number)))
    (cond ((and (= size 32) (= power 1) (<= max-number 1073741828))
           `(ldb (byte 32 32) (* ,number 429496730)))
          ((and (= size 64) (= power 1) (<= max-number 4611686018427387908))
           #+quaviver/math/smallnum
           `(quaviver/math::*/64-64/hi64 ,number 1844674407370955162)
           #-quaviver/math/smallnum
           `(ldb (byte 64 64) (* ,number 1844674407370955162)))
          ((and (= size 32) (= power 2))
           `(ldb (byte 27 37) (* ,number 1374389535)))
          ((and (= size 64) (= power 3) (<= max-number 15534100272597517998))
           #+quaviver/math/smallnum
           `(ash (quaviver/math::*/64-64/hi64 ,number 4722366482869645214) -8)
           #-quaviver/math/smallnum
           `(ldb (byte 56 72) (* ,number 4722366482869645214)))
          (t
           `(floor ,number ,(expt 10 power))))))

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
  (defgeneric min-k/nearest/shorter-interval (type))
  (defgeneric max-k/nearest/shorter-interval (type))
  (defgeneric min-k/nearest/normal-interval (type))
  (defgeneric max-k/nearest/normal-interval (type))
  (defgeneric min-k/left-closed-directed (type))
  (defgeneric max-k/left-closed-directed (type))
  (defgeneric min-k/right-closed-directed (type))
  (defgeneric max-k/right-closed-directed (type))
  (defgeneric beta/nearest/shorter-interval (exponent))
  (defgeneric beta/nearest/normal-interval (exponent kappa))
  (defgeneric beta-1/right-closed-directed (exponent kappa))
  (defgeneric beta-2/right-closed-directed (exponent kappa))
  (defgeneric min-beta/nearest/shorter-interval (type))
  (defgeneric max-beta/nearest/shorter-interval (type))
  (defgeneric min-beta/nearest/normal-interval (type))
  (defgeneric max-beta/nearest/normal-interval (type))
  (defgeneric min-beta/left-closed-directed (type))
  (defgeneric max-beta/left-closed-directed (type))
  (defgeneric min-beta/right-closed-directed (type))
  (defgeneric max-beta/right-closed-directed (type))

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

  (defmethod min-k/nearest/shorter-interval (type)
    (- (floor-log10-expt2-minus-log10-4/3 (quaviver:max-exponent type))))

  (defmethod max-k/nearest/shorter-interval (type)
    (- (floor-log10-expt2-minus-log10-4/3 (quaviver:min-exponent type))))

  (defmethod min-k/nearest/normal-interval (type)
    (- (kappa type) (floor-log10-expt2 (quaviver:max-exponent type))))

  (defmethod max-k/nearest/normal-interval (type)
    (- (kappa type) (floor-log10-expt2 (quaviver:min-exponent type))))

  (defmethod min-k/left-closed-directed (type)
    (min-k/nearest/normal-interval type))

  (defmethod max-k/left-closed-directed (type)
    (max-k/nearest/normal-interval type))

  (defmethod min-k/right-closed-directed (type)
    (min-k/nearest/normal-interval type))

  (defmethod max-k/right-closed-directed (type)
    (- (kappa type) (floor-log10-expt2 (1- (quaviver:min-exponent type)))))

  (defmethod beta/nearest/shorter-interval (exponent)
    (+ exponent (floor-log2-expt10 (- (floor-log10-expt2-minus-log10-4/3 exponent)))))

  (defmethod beta/nearest/normal-interval (exponent kappa)
    (+ exponent (floor-log2-expt10 (- kappa (floor-log10-expt2 exponent)))))

  (defmethod beta-1/right-closed-directed (exponent kappa)
    (beta/nearest/normal-interval exponent kappa))

  (defmethod beta-2/right-closed-directed (exponent kappa)
    (+ exponent (floor-log2-expt10 (- kappa (floor-log10-expt2 (1- exponent))))))

  (defmethod min-beta/nearest/shorter-interval (type)
    (loop for exponent from (quaviver:min-exponent type) to (quaviver:max-exponent type)
          minimize (beta/nearest/shorter-interval exponent)))

  (defmethod max-beta/nearest/shorter-interval (type)
    (loop for exponent from (quaviver:min-exponent type) to (quaviver:max-exponent type)
          maximize (beta/nearest/shorter-interval exponent)))

  (defmethod min-beta/nearest/normal-interval (type)
    (loop with kappa = (kappa type)
          for exponent from (quaviver:min-exponent type) to (quaviver:max-exponent type)
          minimize (beta/nearest/normal-interval exponent kappa)))

  (defmethod max-beta/nearest/normal-interval (type)
    (loop with kappa = (kappa type)
          for exponent from (quaviver:min-exponent type) to (quaviver:max-exponent type)
          maximize (beta/nearest/normal-interval exponent kappa)))

  (defmethod min-beta/left-closed-directed (type)
    (min-beta/nearest/normal-interval type))

  (defmethod max-beta/left-closed-directed (type)
    (max-beta/nearest/normal-interval type))

  (defmethod min-beta/right-closed-directed (type)
    (loop with kappa = (kappa type)
          for exponent from (quaviver:min-exponent type) to (quaviver:max-exponent type)
          minimize (min (beta-1/right-closed-directed exponent kappa)
                        (beta-2/right-closed-directed exponent kappa))))

  (defmethod max-beta/right-closed-directed (type)
    (loop with kappa = (kappa type)
          for exponent from (quaviver:min-exponent type) to (quaviver:max-exponent type)
          maximize (max (beta-1/right-closed-directed exponent kappa)
                        (beta-2/right-closed-directed exponent kappa))))

  (defun count-factors (number divisor)
    (assert (> number 0))
    (assert (> divisor 1))
    (loop with remainder
          do (multiple-value-setq (number remainder) (floor number divisor))
          while (zerop remainder)
          count 1)))

#+(or)
(loop for type in '(single-float double-float)
      collect (list type
                    (list :nearest/shorter-interval
                          (list (min-beta/nearest/shorter-interval type)
                                (max-beta/nearest/shorter-interval type))
                          :nearest/normal-interval
                          (list (min-beta/nearest/normal-interval type)
                                (max-beta/nearest/normal-interval type))
                          :left-closed-directed
                          (list (min-beta/left-closed-directed type)
                                (max-beta/left-closed-directed type))
                          :right-closed-directed
                          (list (min-beta/right-closed-directed type)
                                (max-beta/right-closed-directed type)))))

#+(or) ; =>
((SINGLE-FLOAT
  (:NEAREST/SHORTER-INTERVAL (0 3)
   :NEAREST/NORMAL-INTERVAL (3 6)
   :LEFT-CLOSED-DIRECTED (3 6)
   :RIGHT-CLOSED-DIRECTED (3 7)))
 (DOUBLE-FLOAT
  (:NEAREST/SHORTER-INTERVAL (0 3)
   :NEAREST/NORMAL-INTERVAL (6 9)
   :LEFT-CLOSED-DIRECTED (6 9)
   :RIGHT-CLOSED-DIRECTED (6 10))))

;;; Based on https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L3247-L3551
(defmacro %nearest (client value type hi/2n floor-multiply floor-multiply/evenp)
  (with-accessors ((arithmetic-size quaviver:arithmetic-size)
                   (significand-size quaviver:significand-size)
                   (min-exponent quaviver:min-exponent)
                   (max-exponent quaviver:max-exponent)
                   (kappa kappa)
                   (min-k/si min-k/nearest/shorter-interval)
                   (max-k/si max-k/nearest/shorter-interval)
                   (min-k/ni min-k/nearest/normal-interval)
                   (max-k/ni max-k/nearest/normal-interval)
                   (min-beta/si min-beta/nearest/shorter-interval)
                   (max-beta/si max-beta/nearest/shorter-interval)
                   (min-beta/ni min-beta/nearest/normal-interval)
                   (max-beta/ni max-beta/nearest/normal-interval))
      type
    (progn                              ; for future LET bindings
      `(block %dragonbox
         (let ((significand 0)
               (exponent 0)
               (sign 0)
               (2fc 0))
           (declare ((quaviver/math:arithmetic-word ,arithmetic-size) significand 2fc)
                    ((or (integer ,min-exponent ,max-exponent) keyword) exponent)
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
                      (beta (+ exponent (floor-log2-expt10 (- -k) ,min-k/si ,max-k/si)))
                      (expt10 (quaviver/math:expt ,arithmetic-size 10 -k))
                      ;; Left endpoint
                      (xi (let ((hi64 (,hi/2n expt10 64)))
                            (quaviver/math:hi/64 (- hi64 (ash hi64 ,(- (1+ significand-size))))
                                                 (+ ,significand-size beta))))
                      ;; Right endpoint
                      (zi (let ((hi64 (,hi/2n expt10 64)))
                            (quaviver/math:hi/64 (+ hi64 (ash hi64 ,(- significand-size)))
                                                 (+ ,significand-size beta)))))
                 (declare ((integer ,(- max-k/si) ,(- min-k/si)) -k)
                          ((integer ,min-beta/si ,max-beta/si) beta)
                          ((quaviver/math:arithmetic-word ,arithmetic-size 2) expt10)
                          ((quaviver/math:arithmetic-word ,arithmetic-size) xi zi)
                          (dynamic-extent expt10))
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
                 (when (>= (the (quaviver/math:arithmetic-word ,arithmetic-size)
                                (* 10 significand)) xi)
                   (return-from %dragonbox
                     (values significand (1+ -k) sign)))
                 (setf significand
                       (ash (the (quaviver/math:arithmetic-word ,arithmetic-size)
                                 (1+ (the (quaviver/math:arithmetic-word ,arithmetic-size)
                                          (,hi/2n expt10 (+ ,significand-size 1 beta)))))
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
                    (beta (+ exponent (floor-log2-expt10 (- -k) ,min-k/ni ,max-k/ni)))
                    (expt10 (quaviver/math:expt ,arithmetic-size 10 -k))
                    (deltai (,hi/2n expt10 (1+ beta)))
                    (zi 0)
                    (zi-integer-p nil)
                    (r 0))
               (declare ((integer ,(- max-k/ni) ,(- min-k/ni)) -k)
                        ((integer ,min-beta/ni ,max-beta/ni) beta)
                        ((quaviver/math:arithmetic-word ,arithmetic-size 2) expt10)
                        ((quaviver/math:arithmetic-word ,arithmetic-size) deltai zi r)
                        (boolean zi-integer-p)
                        (dynamic-extent expt10))
               (multiple-value-setq (zi zi-integer-p)
                 (,floor-multiply (logior 2fc 1) expt10 beta))
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
                        (multiple-value-bind (xi-even-p xi-integer-p)
                            (,floor-multiply/evenp (1- 2fc) expt10 beta)
                          (when (zerop (logior (if xi-even-p 1 0)
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
                      (let* ((dist (+ (the (quaviver/math:arithmetic-word ,arithmetic-size)
                                           (- r (ash deltai -1)))
                                      ,(floor (expt 10 kappa) 2)))
                             (approx-y-even-p
                               (logbitp 0 (logxor dist ,(floor (expt 10 kappa) 2)))))
                        (declare ((quaviver/math:arithmetic-word ,arithmetic-size) dist))
                        (multiple-value-bind (dist divisible-p)
                            (floor-by-expt10-divisible-p dist ,kappa ,arithmetic-size)
                          (incf significand dist)
                          (when divisible-p
                            (multiple-value-bind (yi-even-p yi-integer-p)
                                (,floor-multiply/evenp 2fc expt10 beta)
                              (cond ((not (eq yi-even-p approx-y-even-p))
                                     (decf significand))
                                    ((and (prefer-round-down-p ,client significand)
                                          yi-integer-p)
                                     (decf significand)))))))))
               (values significand (+ -k ,kappa) sign))))))))

;;; Based on https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L3553-L3799
(defmacro %directed (client value type hi/2n floor-multiply floor-multiply/evenp)
  (with-accessors ((arithmetic-size quaviver:arithmetic-size)
                   (significand-size quaviver:significand-size)
                   (min-exponent quaviver:min-exponent)
                   (max-exponent quaviver:max-exponent)
                   (kappa kappa)
                   (min-k/left min-k/left-closed-directed)
                   (max-k/left max-k/left-closed-directed)
                   (min-k/right min-k/right-closed-directed)
                   (max-k/right max-k/right-closed-directed)
                   (min-beta/left min-beta/left-closed-directed)
                   (max-beta/left max-beta/left-closed-directed)
                   (min-beta/right min-beta/right-closed-directed)
                   (max-beta/right max-beta/right-closed-directed))
      type
    (progn                              ; for future LET bindings
      `(block %dragonbox
         (let ((significand 0)
               (exponent 0)
               (sign 0)
               (2fc 0))
           (declare ((quaviver/math:arithmetic-word ,arithmetic-size) significand 2fc)
                    ((or (integer ,min-exponent ,max-exponent) keyword) exponent)
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
                     (beta (+ exponent (floor-log2-expt10 (- -k) ,min-k/left ,max-k/left)))
                     (expt10 (quaviver/math:expt ,arithmetic-size 10 -k))
                     (deltai (,hi/2n expt10 (1+ beta)))
                     (xi 0)
                     (xi-integer-p nil)
                     (r 0))
                (declare ((integer ,(- max-k/left) ,(- min-k/left)) -k)
                         ((integer ,min-beta/left ,max-beta/left) beta)
                         ((quaviver/math:arithmetic-word ,arithmetic-size 2) expt10)
                         ((quaviver/math:arithmetic-word ,arithmetic-size) deltai xi r)
                         (boolean xi-integer-p)
                         (dynamic-extent expt10))
                (multiple-value-setq (xi xi-integer-p)
                  (,floor-multiply 2fc expt10 beta))
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
                         (multiple-value-bind (zi-even-p zi-integer-p)
                             (,floor-multiply/evenp (+ 2fc 2) expt10 beta)
                           (when (or zi-even-p zi-integer-p)
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
                     (beta (+ exponent (floor-log2-expt10 (- -k) ,min-k/right ,max-k/right)))
                     (expt10 (quaviver/math:expt ,arithmetic-size 10 -k))
                     (deltai (,hi/2n expt10 (1+ (- beta (if shorter-interval-p 1 0)))))
                     (zi (nth-value 0 (,floor-multiply 2fc expt10 beta)))
                     (r 0))
                (declare ((integer ,(- max-k/right) ,(- min-k/right)) -k)
                         ((integer ,min-beta/right ,max-beta/right) beta)
                         ((quaviver/math:arithmetic-word ,arithmetic-size 2) expt10)
                         ((quaviver/math:arithmetic-word ,arithmetic-size) deltai zi r)
                         (dynamic-extent expt10))
                ;; Step 2: Try larger divisor; remove trailing zeros if necessary
                (setf significand (floor-by-expt10 ; base 10 significand from here on out
                                   zi ,(1+ kappa) ,arithmetic-size
                                   ,(1- (* (expt 10 (1+ kappa)) (ash 1 significand-size))))
                      r (- zi (* ,(expt 10 (1+ kappa)) significand)))
                (block nil
                  (cond ((> r deltai) (return))
                        ((= r deltai)
                         (multiple-value-bind (xi-even-p xi-integer-p)
                             (,floor-multiply/evenp (- 2fc (if shorter-interval-p 1 2))
                                                    expt10 beta)
                           (declare (ignore xi-integer-p))
                           (unless xi-even-p
                             (return)))))
                  (return-from %dragonbox
                    (values significand (+ -k ,kappa 1) sign)))
                ;; Step 3: Find the significand with the smaller divisor
                (setf significand (+ (* 10 significand)
                                     (floor-by-expt10-small r ,kappa ,arithmetic-size)))
                (values significand (+ -k ,kappa) sign)))))))))

#+clisp
(defmethod quaviver:float-integer ((client nearest-client) (base (eql 10)) value)
  (declare (optimize speed))
  (etypecase value
    (short-float
     (%nearest client value
               short-float
               quaviver/math:hi/64
               quaviver/math:floor-multiply/32-64q64
               quaviver/math:floor-multiply/evenp/32-64q64))
    (single-float
     (%nearest client value
               single-float
               quaviver/math:hi/64
               quaviver/math:floor-multiply/32-64q64
               quaviver/math:floor-multiply/evenp/32-64q64))
    (double-float
     (%nearest client value
               double-float
               quaviver/math:hi/hi64/128
               quaviver/math:floor-multiply/64-128q128
               quaviver/math:floor-multiply/evenp/64-128q128))))

#+(and (not clisp) quaviver/short-float)
(defmethod quaviver:float-integer ((client nearest-client) (base (eql 10)) (value short-float))
  (declare (optimize speed))
  (%nearest client value
            short-float
            quaviver/math:hi/64
            quaviver/math:floor-multiply/32-64q64
            quaviver/math:floor-multiply/evenp/32-64q64))

#-clisp
(defmethod quaviver:float-integer ((client nearest-client) (base (eql 10)) (value single-float))
  (declare (optimize speed))
  (%nearest client value
            single-float
            quaviver/math:hi/64
            quaviver/math:floor-multiply/32-64q64
            quaviver/math:floor-multiply/evenp/32-64q64))

#-clisp
(defmethod quaviver:float-integer ((client nearest-client) (base (eql 10)) (value double-float))
  (declare (optimize speed))
  (%nearest client value
            double-float
            quaviver/math:hi/hi64/128
            quaviver/math:floor-multiply/64-128q128
            quaviver/math:floor-multiply/evenp/64-128q128))

#+clisp
(defmethod quaviver:float-integer ((client directed-client) (base (eql 10)) value)
  (declare (optimize speed))
  (etypecase value
    (short-float
     (%directed client value
               short-float
               quaviver/math:hi/64
               quaviver/math:floor-multiply/32-64q64
               quaviver/math:floor-multiply/evenp/32-64q64))
    (single-float
     (%directed client value
               single-float
               quaviver/math:hi/64
               quaviver/math:floor-multiply/32-64q64
               quaviver/math:floor-multiply/evenp/32-64q64))
    (double-float
     (%directed client value
               double-float
               quaviver/math:hi/hi64/128
               quaviver/math:floor-multiply/64-128q128
               quaviver/math:floor-multiply/evenp/64-128q128))))

#+(and (not clisp) quaviver/short-float)
(defmethod quaviver:float-integer ((client directed-client) (base (eql 10)) (value short-float))
  (declare (optimize speed))
  (%directed client value
             short-float
             quaviver/math:hi/64
             quaviver/math:floor-multiply/32-64q64
             quaviver/math:floor-multiply/evenp/32-64q64))

#-clisp
(defmethod quaviver:float-integer ((client directed-client) (base (eql 10)) (value single-float))
  (declare (optimize speed))
  (%directed client value
             single-float
             quaviver/math:hi/64
             quaviver/math:floor-multiply/32-64q64
             quaviver/math:floor-multiply/evenp/32-64q64))

#-clisp
(defmethod quaviver:float-integer ((client directed-client) (base (eql 10)) (value double-float))
  (declare (optimize speed))
  (%directed client value
             double-float
             quaviver/math:hi/hi64/128
             quaviver/math:floor-multiply/64-128q128
             quaviver/math:floor-multiply/evenp/64-128q128))
