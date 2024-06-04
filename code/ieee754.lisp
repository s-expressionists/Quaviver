;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT AND BSD-3-Clause
;;;;
;;;; The implementations of QUAVIVER:FLOAT-TO-BITS were ported, with
;;;; modifications, from Nibbles [1], which is licensed under the
;;;; BSD-3-Clause license.
;;;; Any original code herein is licensed under the MIT license (Expat).
;;;;
;;;; [1]: https://github.com/sharplispers/nibbles

(cl:defpackage #:quaviver/ieee754
  (:use #:common-lisp)
  (:export #:client))
(cl:in-package #:quaviver/ieee754)

(defclass client () ())

;;; Based on NIBBLES::IEEE-SINGLE-SET/BE [1].
;;;
;;; https://github.com/sharplispers/nibbles/blob/6faa72064a361f916e5e545edde9ba5c65721a82/vectors.lisp#L93
(defmethod quaviver:float-to-bits ((client client) (value single-float))
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
(defmethod quaviver:float-to-bits ((client client) (value double-float))
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
