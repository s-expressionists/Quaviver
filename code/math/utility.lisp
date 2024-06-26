(in-package #:quaviver/math)

(defun compute-expt (k-min k-max width &optional (base 10))
  (make-array (- k-max k-min -1)
              :initial-contents
              (loop for k from k-min upto k-max
                    for l = (expt base (abs k))
                    collect (ldb (byte width 0)
                                 (if (minusp k)
                                     (ceiling (/ (ash 1 (+ width (integer-length l) -1))
                                                 l))
                                     (let ((shift (- width (integer-length l))))
                                       (if (minusp shift)
                                           (ceiling (/ l (ash 1 (abs shift))))
                                           (ceiling (* (ash 1 shift)
                                                       l)))))))))

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
