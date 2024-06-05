;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT AND BSD-3-Clause
;;;;
;;;; The implementations of QUAVIVER:BITS-FLOAT and QUAVIVER:FLOAT-BITS
;;;; were ported, with modifications, from Nibbles [1], which is
;;;; licensed under the BSD-3-Clause license.
;;;; Any original code herein is licensed under the MIT license (Expat).
;;;;
;;;; [1]: https://github.com/sharplispers/nibbles

(cl:defpackage #:quaviver/ieee754
  (:use #:common-lisp)
  (:export #:client))
(cl:in-package #:quaviver/ieee754)

(defclass client () ())

;;; Based on NIBBLES::IEEE-SINGLE-REF/BE [1].
;;;
;;; https://github.com/sharplispers/nibbles/blob/6faa72064a361f916e5e545edde9ba5c65721a82/vectors.lisp#L60
(defmethod quaviver:bits-float
    ((client client) (result-type (eql 'single-float)) (bits integer))
  #+abcl
  (system:make-single-float bits)
  #+allegro
  (let ((upper (ldb (byte 16 16) bits))
        (lower (ldb (byte 16 0) bits)))
    (excl:shorts-to-single-float upper lower))
  #+ccl
  (ccl::host-single-float-from-unsigned-byte-32 bits)
  #+clasp
  (ext:bits-to-single-float bits)
  #+cmu
  (kernel:make-single-float bits)
  #+ecl
  (system:bits-single-float bits)
  #+lispworks
  (let ((v (sys:make-typed-aref-vector 4)))
    (declare (optimize (speed 3) (float 0) (safety 0))
             (dynamic-extent v))
    (setf (sys:typed-aref '(unsigned-byte 32) v 0) bits)
    (sys:typed-aref 'single-float v 0))
  #+mezzano
  (mezzano.extensions:ieee-binary32-to-single-float bits)
  #+sbcl
  (sb-kernel:make-single-float bits)
  #-(or abcl allegro ccl clasp cmu ecl lispworks mezzano sbcl)
  ;; Based on NIBBLES::MAKE-SINGLE-FLOAT [1].
  ;;
  ;; https://github.com/sharplispers/nibbles/blob/6faa72064a361f916e5e545edde9ba5c65721a82/float.lisp#L5
  (let ((exponent-bits (ldb (byte 8 23) bits)))
    (when (= exponent-bits 255)
      (error "Infinities and NaNs are unsupported."))
    (let ((sign (if (zerop (ldb (byte 1 31) bits)) 1f0 -1f0))
          (significand (logior (ldb (byte 23 0) bits)
                               (if (zerop exponent-bits) 0 (ash 1 23))))
          (exponent (if (zerop exponent-bits) -126 (- exponent-bits 127))))
      (* sign (scale-float (float significand 1f0) (- exponent 23))))))

;;; Based on NIBBLES::IEEE-DOUBLE-REF/BE [1].
;;;
;;; https://github.com/sharplispers/nibbles/blob/6faa72064a361f916e5e545edde9ba5c65721a82/vectors.lisp#L231
(defmethod quaviver:bits-float
    ((client client) (result-type (eql 'double-float)) (bits integer))
  #+abcl
  (system:make-double-float bits)
  #+allegro
  (let ((us3 (ldb (byte 16 48) bits))
        (us2 (ldb (byte 16 32) bits))
        (us1 (ldb (byte 16 16) bits))
        (us0 (ldb (byte 16 0) bits)))
    (excl:shorts-to-double-float us3 us2 us1 us0))
  #+ccl
  (let ((upper (ldb (byte 32 32) bits))
        (lower (ldb (byte 32 0) bits)))
    (ccl::double-float-from-bits upper lower))
  #+clasp
  (ext:bits-to-double-float bits)
  #+cmu
  (let ((upper (ldb (byte 32 32) bits))
        (lower (ldb (byte 32 0) bits)))
    (kernel:make-double-float upper lower))
  #+ecl
  (system:bits-double-float bits)
  #+lispworks
  (let ((upper (ldb (byte 32 32) bits))
        (lower (ldb (byte 32 0) bits))
        (v (sys:make-typed-aref-vector 8)))
    (declare (optimize (speed 3) (float 0) (safety 0))
             (dynamic-extent v))
    #+little-endian
    (setf (sys:typed-aref '(unsigned-byte 32) v 0) lower
          (sys:typed-aref '(unsigned-byte 32) v 4) upper)
    #-little-endian
    (setf (sys:typed-aref '(unsigned-byte 32) v 0) upper
          (sys:typed-aref '(unsigned-byte 32) v 4) lower)
    (sys:typed-aref 'double-float v 0))
  #+mezzano
  (mezzano.extensions:ieee-binary64-to-double-float bits)
  #+sbcl
  (let ((upper (ldb (byte 32 32) bits))
        (lower (ldb (byte 32 0) bits)))
    (sb-kernel:make-double-float upper lower))
  #-(or abcl allegro ccl clasp cmu ecl lispworks mezzano sbcl)
  ;; Based on NIBBLES::MAKE-DOUBLE-FLOAT [1].
  ;;
  ;; https://github.com/sharplispers/nibbles/blob/6faa72064a361f916e5e545edde9ba5c65721a82/float.lisp#L14
  (let ((exponent-bits (ldb (byte 11 52) bits)))
    (when (= exponent-bits 2047)
      (error "Infinities and NaNs are unsupported."))
    (let ((sign (if (zerop (ldb (byte 1 63) bits)) 1d0 -1d0))
          (significand (logior (ldb (byte 52 0) bits)
                               (if (zerop exponent-bits) 0 (ash 1 52))))
          (exponent (if (zerop exponent-bits) -1022 (- exponent-bits 1023))))
      (* sign (scale-float (float significand 1d0) (- exponent 52))))))

;;; Based on NIBBLES::IEEE-SINGLE-SET/BE [1].
;;;
;;; https://github.com/sharplispers/nibbles/blob/6faa72064a361f916e5e545edde9ba5c65721a82/vectors.lisp#L93
(defmethod quaviver:float-bits ((client client) (value single-float))
  #+abcl
  (system:single-float-bits value)
  #+allegro
  (multiple-value-bind (upper lower) (excl:single-float-to-shorts value)
    (logior (ash upper 16) lower))
  #+ccl
  (ccl::single-float-bits value)
  #+clasp
  (ext:single-float-to-bits value)
  #+cmu
  (kernel:single-float-bits value)
  #+ecl
  (system:single-float-bits value)
  #+lispworks
  (let ((v (sys:make-typed-aref-vector 4)))
    (declare (optimize (speed 3) (float 0) (safety 0))
             (dynamic-extent v))
    (setf (sys:typed-aref 'single-float v 0) value)
    (sys:typed-aref '(unsigned-byte 32) v 0))
  #+mezzano
  (mezzano.extensions:single-float-to-ieee-binary32 value)
  #+sbcl
  (sb-kernel:single-float-bits value)
  #-(or abcl allegro ccl clasp cmu ecl lispworks mezzano sbcl)
  ;; Based on NIBBLES::SINGLE-FLOAT-BITS [1].
  ;;
  ;; https://github.com/sharplispers/nibbles/blob/6faa72064a361f916e5e545edde9ba5c65721a82/float.lisp#L26
  (multiple-value-bind (significand exponent sign) (decode-float value)
    (let ((sign-bit (if (plusp sign) 0 1))
          (exponent-bits (if (zerop significand) 0 (+ exponent 127 -1)))
          (significand-bits (floor (* #.(expt 2f0 24) significand))))
      (when (<= exponent-bits 0)
        (setf significand-bits (ash significand-bits (1- exponent-bits)))
        (setf exponent-bits 0))
      (logior (ash sign-bit 31) (ash exponent-bits 23) (ldb (byte 23 0) significand-bits)))))

;;; Based on NIBBLES::IEEE-DOUBLE-SET/BE [1].
;;;
;;; https://github.com/sharplispers/nibbles/blob/6faa72064a361f916e5e545edde9ba5c65721a82/vectors.lisp#L290
(defmethod quaviver:float-bits ((client client) (value double-float))
  #+abcl
  (let ((upper (system:double-float-high-bits value))
        (lower (system:double-float-low-bits value)))
    (logior (ash upper 32) lower))
  #+allegro
  (multiple-value-bind (us3 us2 us1 us0) (excl:double-float-to-shorts value)
    (logior (ash us3 48) (ash us2 32) (ash us1 16) us0))
  #+ccl
  (multiple-value-bind (upper lower) (ccl::double-float-bits value)
    (logior (ash upper 32) lower))
  #+clasp
  (ext:double-float-to-bits value)
  #+cmu
  (let ((upper (kernel:double-float-high-bits value))
        (lower (kernel:double-float-low-bits value)))
    (logior (ash upper 32) lower))
  #+ecl
  (system:double-float-bits value)
  #+lispworks
  (let ((v (sys:make-typed-aref-vector 8)))
    (declare (optimize (speed 3) (float 0) (safety 0))
             (dynamic-extent v))
    (setf (sys:typed-aref 'double-float v 0) value)
    #+little-endian
    (let ((upper (sys:typed-aref '(unsigned-byte 32) v 4))
          (lower (sys:typed-aref '(unsigned-byte 32) v 0)))
      (logior (ash upper 32) lower))
    #-little-endian
    (let ((upper (sys:typed-aref '(unsigned-byte 32) v 0))
          (lower (sys:typed-aref '(unsigned-byte 32) v 4)))
      (logior (ash upper 32) lower)))
  #+mezzano
  (mezzano.extensions:double-float-to-ieee-binary64 value)
  #+sbcl
  (let ((upper (sb-kernel:double-float-high-bits value))
        (lower (sb-kernel:double-float-low-bits value)))
    (logior (ash upper 32) lower))
  #-(or abcl allegro ccl clasp cmu ecl lispworks mezzano sbcl)
  ;; Based on NIBBLES::DOUBLE-FLOAT-BITS [1].
  ;;
  ;; https://github.com/sharplispers/nibbles/blob/6faa72064a361f916e5e545edde9ba5c65721a82/float.lisp#L37
  (multiple-value-bind (significand exponent sign) (decode-float value)
    (let ((sign-bit (if (plusp sign) 0 1))
          (exponent-bits (if (zerop significand) 0 (+ exponent 1023 -1)))
          (significand-bits (floor (* #.(expt 2d0 53) significand))))
      (when (<= exponent-bits 0)
        (setf significand-bits (ash significand-bits (1- exponent-bits)))
        (setf exponent-bits 0))
      (logior (ash sign-bit 63) (ash exponent-bits 52) (ldb (byte 52 0) significand-bits)))))
