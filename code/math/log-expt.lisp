(in-package #:quaviver.math)

(declaim (type (simple-array t (35 35))
               *log-expt*)
         (type (simple-vector 35)
               *log-3/4*)
         (ftype (function (fixnum fixnum fixnum &optional boolean)
                          (values fixnum &optional))
                floor-log-expt ceiling-log-expt)
         (inline floor-log-expt
                 ceiling-log-expt))

(defconstant +min-base+ 2)

(defconstant +max-base+ 36)

(defconstant +log-expt-shift+ 22)

(defun compute-log-expt ()
  (make-array (list (- +max-base+ +min-base+ -1)
                    (- +max-base+ +min-base+ -1))
              :initial-contents
              (loop for log-base from +min-base+ upto +max-base+
                    collect (loop for expt-base from +min-base+ upto +max-base+
                                  collect (floor (* (log (coerce expt-base 'double-float)
                                                         log-base)
                                                    (ash 1 +log-expt-shift+)))))))

(defun compute-log-constant (b)
  (make-array  (- +max-base+ +min-base+ -1)
              :initial-contents
              (loop with v = (coerce (/ b) 'double-float)
                    for log-base from +min-base+ upto +max-base+
                    collect (- (floor (* (log v log-base) (ash 1 +log-expt-shift+)))))))

(defvar *log-expt* (compute-log-expt))

(defvar *log-3/4* (compute-log-constant 3/4))

(defun floor-log-expt (log-base expt-base exp &optional three-quarters-p)
  (declare (optimize speed))
  (ash (+ (* exp (the fixnum
                      (aref *log-expt*
                            (- log-base +min-base+)
                            (- expt-base +min-base+))))
          (if three-quarters-p
              (the fixnum (svref *log-3/4* (- log-base +min-base+)))
              0))
       (- +log-expt-shift+)))

(define-compiler-macro floor-log-expt
    (&whole whole log-base expt-base exp &optional three-quarters-p)
  (if (or (not (constantp log-base))
          (not (constantp expt-base)))
      whole
      (let ((multiplier (aref *log-expt*
                              (- log-base +min-base+)
                              (- expt-base +min-base+)))
            (offset (svref *log-3/4* (- log-base +min-base+)))
            (shift (- +log-expt-shift+)))
        (cond ((null three-quarters-p)
               `(ash (* ,exp ,multiplier) ,shift))
              ((constantp three-quarters-p)
               `(ash (+ (* ,exp ,multiplier) ,offset)
                     ,shift))
              (t
               `(ash (+ (* ,exp ,multiplier)
                        (if ,three-quarters-p
                            ,offset
                            0))
                     ,shift))))))

(defun ceiling-log-expt (log-base expt-base exp &optional three-quarters-p)
  (values (ceiling (+ (* exp
                         (the fixnum
                              (aref *log-expt*
                                    (- log-base +min-base+)
                                    (- expt-base +min-base+))))
                      (if three-quarters-p
                          (the fixnum (svref *log-3/4* (- log-base +min-base+)))
                          0))
                   (ash 1 +log-expt-shift+))))

(define-compiler-macro ceiling-log-expt
    (&whole whole log-base expt-base exp &optional three-quarters-p)
  (if (or (not (constantp log-base))
          (not (constantp expt-base)))
      whole
      (let ((multiplier (aref *log-expt*
                              (- log-base +min-base+)
                              (- expt-base +min-base+)))
            (offset (svref *log-3/4* (- log-base +min-base+)))
            (divisor (ash 1 +log-expt-shift+)))
        (cond ((null three-quarters-p)
               `(values (ceiling (* ,exp ,multiplier) ,divisor)))
              ((constantp three-quarters-p)
               `(values (ceiling (+ (* ,exp ,multiplier) ,offset)
                                 ,divisor)))
              (t
               `(values (ceiling (+ (* ,exp ,multiplier)
                                    (if ,three-quarters-p
                                        ,offset
                                        0))
                                 ,divisor)))))))
