(in-package #:quaviver/math)

(defmacro define-expt (&key types (base 10))
  `(progn
     ,@(loop for (arithmetic-size bound) on (compute-expt-bounds types base) by #'cddr
             for fun-name = (alexandria:symbolicate
                             '#:expt/ (write-to-string arithmetic-size)
                             "-" (write-to-string base))
             for bound-name = (alexandria:symbolicate
                               '#:*expt/ (write-to-string arithmetic-size)
                               "-" (write-to-string base)
                               '#:/bound*)
             for values-name = (alexandria:symbolicate
                                '#:*expt/ (write-to-string arithmetic-size)
                                "-" (write-to-string base)
                                '#:/values*)
             nconc `((declaim (ftype (function (fixnum) (arithmetic-word ,arithmetic-size 2))
                                     ,fun-name)
                              (inline ,fun-name))

                     (defvar ,bound-name ,bound)

                     (defvar ,values-name ,(compute-expt (- bound) bound
                                                         (* 2 arithmetic-size)
                                                         base))

                     (defun ,fun-name (power)
                       (svref ,values-name
                              (- ,bound-name power)))))))

(define-expt :types (#+quaviver/short-float
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
        while)))
    (otherwise
     whole)))
