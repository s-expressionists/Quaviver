;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT

(in-package #:quaviver.math)

(defmacro define-expt (&key arithmetic-sizes types (base 10))
  `(progn
     ,@(loop for arithmetic-size in arithmetic-sizes
             for bound = (or (loop for type in types
                                   when (= arithmetic-size (quaviver:arithmetic-size type))
                                     maximize (+ (ceiling-log-expt base 2
                                                                   (max (quaviver:max-exponent type)
                                                                        (- (quaviver:min-exponent type))))
                                                 (ceiling-log-expt base 2
                                                                   (- (quaviver:arithmetic-size type)
                                                                      (quaviver:significand-size type)
                                                                      1))))
                             0)
             for fun-name = (alexandria:symbolicate
                             '#:expt/ (write-to-string arithmetic-size)
                             "-" (write-to-string base))
             for bound-name = (alexandria:symbolicate
                               '#:+expt/ (write-to-string arithmetic-size)
                               "-" (write-to-string base)
                               '#:/bound+)
             for values-name = (alexandria:symbolicate
                                '#:*expt/ (write-to-string arithmetic-size)
                                "-" (write-to-string base)
                                '#:/values*)
             nconc `((declaim (ftype (function (fixnum) (arithmetic-word ,arithmetic-size 2))
                                     ,fun-name)
                              (type (simple-vector ,(1+ (* 2 bound)))
                                    ,values-name)
                              (inline ,fun-name))

                     (defconstant ,bound-name ,bound)

                     (#-sbcl defvar #+sbcl sb-ext:defglobal
                         ,values-name ,(compute-expt (- bound) bound
                                                         (* 2 arithmetic-size)
                                                         base))
                     #+sbcl (declaim (sb-ext:always-bound ,values-name))

                     (defun ,fun-name (power)
                       (svref ,values-name
                              (- ,bound-name power)))))))

(define-expt
  :arithmetic-sizes (32 64 128 256)
  :types (#+quaviver/short-float
          short-float
          single-float
          double-float
          #+quaviver/long-float
          long-float))

(defun expt (arithmetic-size base power)
  (ecase base
    (10
     (ecase arithmetic-size
       (32
        (expt/32-10 power))
       (64
        (expt/64-10 power))
       (128
        (expt/128-10 power))
       (256
        (expt/256-10 power))))))

(define-compiler-macro expt (&whole whole arithmetic-size base power)
  (case base
    (10
     (case arithmetic-size
       (32
        `(expt/32-10 ,power))
       (64
        `(expt/64-10 ,power))
       (128
        `(expt/128-10 ,power))
       (256
        `(expt/256-10 ,power))
       (otherwise
        whole)))
    (otherwise
     whole)))
