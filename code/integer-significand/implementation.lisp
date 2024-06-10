(in-package #:quaviver/integer-significand)

(defclass client () ())

(defmethod quaviver:float-decimal :around ((client client) value)
  (declare (ignore value))
  (multiple-value-bind (significand exponent sign)
      (call-next-method)
    (if (zerop significand)
        (values #(0) exponent sign)
        (prog (digits digit)
         next
           (unless (zerop significand)
             (multiple-value-setq (significand digit) (floor significand 10))
             (if (and (zerop digit)
                      (null digits))
                 (incf exponent)
                 (push digit digits))
             (go next))
           (return (values (coerce digits 'vector)
                           exponent
                           sign))))))
