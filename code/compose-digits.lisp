;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT
;;;;

(in-package #:quaviver)

(defun %compose-digits/vector (base value)
  (declare (optimize speed))
  (if (zerop value)
      #(0)
      (loop with digits = (make-array (quaviver.math:count-digits base value))
            with digit
            for i from (1- (length digits)) downto 0
            finally (return digits)
            do (multiple-value-setq (value digit)
                 (floor value 10))
               (setf (aref digits i) digit))))

(defun %compose-digits/base-string (base value)
  (declare (optimize speed))
  (if (zerop value)
      #(0)
      (loop with digits = (make-string (quaviver.math:count-digits base value)
                                       :element-type 'base-char)
            with digit
            for i from (1- (length digits)) downto 0
            finally (return digits)
            do (multiple-value-setq (value digit)
                 (floor value 10))
               (setf (aref digits i) (digit-char digit)))))

(defun %compose-digits/string (base value)
  (declare (optimize speed))
  (if (zerop value)
      #(0)
      (loop with digits = (make-string (quaviver.math:count-digits base value)
                                       :element-type 'character)
            with digit
            for i from (1- (length digits)) downto 0
            finally (return digits)
            do (multiple-value-setq (value digit)
                 (floor value 10))
               (setf (aref digits i) (digit-char digit)))))

(defun %compose-digits/list (base value)
  (declare (optimize speed))
  (if (zerop value)
      (list 0)
      (prog (digits digit)
       next
         (unless (zerop value)
           (multiple-value-setq (value digit) (floor value base))
           (push digit digits)
           (go next))
         (return digits))))

(defmethod compose-digits (result-type base value)
  (cond ((subtypep result-type 'base-string)
         (%compose-digits/base-string base value))
        ((subtypep result-type 'string)
         (%compose-digits/string base value))
        ((subtypep result-type 'vector)
         (%compose-digits/vector base value))
        ((subtypep result-type 'list)
         (%compose-digits/list base value))
        (t
         (error "Unable to compose digits for type ~a" result-type))))
