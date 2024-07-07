;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT

(in-package #:quaviver/math)

(defun make-arithmetic-word (value width)
  (declare (ignorable width))
  #+quaviver/math/smallnum
  (if (= width 128)
      (make-array 2 :element-type '(unsigned-byte 64)
                    :initial-contents
                    (list (ldb (byte 64 64) value)
                          (ldb (byte 64 0) value)))
      value)
  #-quaviver/math/smallnum
  value)

(defun compute-expt (k-min k-max width &optional (base 10))
  (make-array (- k-max k-min -1)
              :initial-contents
              (loop for k from k-min upto k-max
                    for l = (cl:expt base (abs k))
                    collect (make-arithmetic-word
                             (ldb (byte width 0)
                                  (if (minusp k)
                                      (ceiling (/ (ash 1 (+ width (integer-length l) -1))
                                                  l))
                                      (let ((shift (- width (integer-length l))))
                                        (if (minusp shift)
                                            (ceiling (/ l (ash 1 (abs shift))))
                                            (ceiling (* (ash 1 shift)
                                                        l))))))
                             width))))

(defun compute-expt-bounds (types base)
  (loop with tables = nil
        for type in types
        for arithmetic-size = (quaviver:arithmetic-size type)
        for bound = (+ (ceiling-log-expt base 2
                                         (max (quaviver:max-exponent type)
                                              (- (quaviver:min-exponent type))))
                       (ceiling-log-expt base 2
                                         (- (quaviver:arithmetic-size type)
                                            (quaviver:significand-size type)
                                            1)))
        finally (return tables)
        do (setf (getf tables arithmetic-size)
                 (max (getf tables arithmetic-size 0)
                      bound))))

(defun compute-log-expt (min-base max-base shift)
  (make-array (list (- max-base min-base -1)
                    (- max-base min-base -1))
              :initial-contents
              (loop for log-base from min-base upto max-base
                    collect (loop for expt-base from min-base upto max-base
                                  collect (floor (* (log (coerce expt-base 'double-float)
                                                         log-base)
                                                    (ash 1 shift)))))))

(defun compute-log-3/4 (min-base max-base shift)
  (make-array  (- max-base min-base -1)
              :initial-contents
              (loop for log-base from min-base upto max-base
                    collect (- (floor (* (log (coerce 4/3 'double-float) log-base) (ash 1 shift)))))))
