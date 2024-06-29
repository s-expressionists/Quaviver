;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT AND BSD-3-Clause
;;;;
;;;; The implementations of QUAVIVER:BITS-FLOAT and QUAVIVER:FLOAT-BITS
;;;; were ported, with modifications, from Nibbles [1], which is
;;;; licensed under the BSD-3-Clause license.
;;;; Any original code herein is licensed under the MIT license (Expat).
;;;;
;;;; [1]: https://github.com/sharplispers/nibbles

(cl:in-package #:quaviver/ieee754)

(defclass client () ())

#-(or abcl allegro ccl clasp cmucl ecl lispworks mezzano sbcl)
(defmethod quaviver:bits-float
    ((client client) (result-type (eql 'single-float)) (bits integer))
  ;; Based on NIBBLES::MAKE-SINGLE-FLOAT [1].
  ;;
  ;; [1]: https://github.com/sharplispers/nibbles/blob/6faa72064a361f916e5e545edde9ba5c65721a82/float.lisp#L5
  (let ((exponent-bits (ldb (byte 8 23) bits)))
    (when (= exponent-bits 255)
      (error "Infinities and NaNs are unsupported."))
    (let ((sign (if (zerop (ldb (byte 1 31) bits)) 1f0 -1f0))
          (significand (logior (ldb (byte 23 0) bits)
                               (if (zerop exponent-bits) 0 (ash 1 23))))
          (exponent (if (zerop exponent-bits) -126 (- exponent-bits 127))))
      (* sign (scale-float (float significand 1f0) (- exponent 23))))))

#-(or abcl allegro ccl clasp cmucl ecl lispworks mezzano sbcl)
(defmethod quaviver:bits-float
    ((client client) (result-type (eql 'double-float)) (bits integer))
  ;; Based on NIBBLES::MAKE-DOUBLE-FLOAT [1].
  ;;
  ;; [1]: https://github.com/sharplispers/nibbles/blob/6faa72064a361f916e5e545edde9ba5c65721a82/float.lisp#L14
  (let ((exponent-bits (ldb (byte 11 52) bits)))
    (when (= exponent-bits 2047)
      (error "Infinities and NaNs are unsupported."))
    (let ((sign (if (zerop (ldb (byte 1 63) bits)) 1d0 -1d0))
          (significand (logior (ldb (byte 52 0) bits)
                               (if (zerop exponent-bits) 0 (ash 1 52))))
          (exponent (if (zerop exponent-bits) -1022 (- exponent-bits 1023))))
      (* sign (scale-float (float significand 1d0) (- exponent 52))))))

#-(or abcl allegro ccl clasp cmucl ecl lispworks mezzano sbcl)
(defmethod quaviver:float-bits ((client client) (value single-float))
  ;; Based on NIBBLES::SINGLE-FLOAT-BITS [1].
  ;;
  ;; [1]: https://github.com/sharplispers/nibbles/blob/6faa72064a361f916e5e545edde9ba5c65721a82/float.lisp#L26
  (multiple-value-bind (significand exponent sign) (decode-float value)
    (let ((sign-bit (if (plusp sign) 0 1))
          (exponent-bits (if (zerop significand) 0 (+ exponent 127 -1)))
          (significand-bits (floor (* #.(expt 2f0 24) significand))))
      (when (<= exponent-bits 0)
        (setf significand-bits (ash significand-bits (1- exponent-bits)))
        (setf exponent-bits 0))
      (logior (ash sign-bit 31) (ash exponent-bits 23) (ldb (byte 23 0) significand-bits)))))

#-(or abcl allegro ccl clasp cmucl ecl lispworks mezzano sbcl)
(defmethod quaviver:float-bits ((client client) (value double-float))
  ;; Based on NIBBLES::DOUBLE-FLOAT-BITS [1].
  ;;
  ;; [1]: https://github.com/sharplispers/nibbles/blob/6faa72064a361f916e5e545edde9ba5c65721a82/float.lisp#L37
  (multiple-value-bind (significand exponent sign) (decode-float value)
    (let ((sign-bit (if (plusp sign) 0 1))
          (exponent-bits (if (zerop significand) 0 (+ exponent 1023 -1)))
          (significand-bits (floor (* #.(expt 2d0 53) significand))))
      (when (<= exponent-bits 0)
        (setf significand-bits (ash significand-bits (1- exponent-bits)))
        (setf exponent-bits 0))
      (logior (ash sign-bit 63) (ash exponent-bits 52) (ldb (byte 52 0) significand-bits)))))
