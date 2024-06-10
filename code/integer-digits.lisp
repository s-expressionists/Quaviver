(in-package #:quaviver)

(defun integer-digits (value)
  (prog (digits digit)
   next
     (unless (zerop value)
       (multiple-value-setq (value digit) (floor value 10))
       (push digit digits)
       (go next))
     (return (coerce digits 'vector))))
