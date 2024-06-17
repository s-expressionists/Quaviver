(in-package #:quaviver/math)

(defun compute-expt10 (k-min k-max width &optional (base 10))
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
