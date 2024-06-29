(in-package #:quaviver/liebler)

(defclass client ()
  ())

(defmacro %liebler (client result-type significand exponent sign expt10 round-to-odd)
  (with-accessors ((arithmetic-size quaviver:arithmetic-size)
                   (significand-size quaviver:significand-size)
                   (min-exponent quaviver:min-exponent)
                   (max-exponent quaviver:max-exponent))
      result-type
    `(if (or (not (numberp ,exponent))
             (zerop ,significand))
         (quaviver:integer-float ,client ',result-type 2
                                 ,significand ,exponent ,sign)
         (let* ((k (quaviver/math:floor-log-expt 2 10 ,exponent))
                (q (+ k (- (integer-length ,exponent) ,significand-size)))
                (shift (- ,arithmetic-size (integer-length ,significand))))
           ;; The following overflow and underflow checks are not
           ;; strict checks. Stricter checks will happen in
           ;; integer-float/2. These are here to protect the expt10
           ;; table lookup from an out of bounds error.
           (when (> q ,(+ max-exponent
                          (quaviver/math:ceiling-log-expt 2 10 1)))
             (error 'floating-point-overflow
                    :operation 'quaviver:integer-float
                    :operands (list ,client ',result-type 10
                                    ,significand ,exponent ,sign)))
           (when (< q ,(- min-exponent
                          (quaviver/math:ceiling-log-expt 2 10 1)))
             (error 'floating-point-underflow
                    :operation 'quaviver:integer-float
                    :operands (list ,client ',result-type 10
                                    ,significand ,exponent ,sign)))
           (setf ,significand (,round-to-odd (,expt10 (- ,exponent))
                                             (ash ,significand shift))
                 k (- k -1 shift)
                 shift (- ,significand-size (integer-length ,significand)))
           (quaviver:integer-float ,client ',result-type 2
                                   (round ,significand (ash 1 (- shift)))
                                   (- k shift)
                                   ,sign)))))

(defmethod quaviver:integer-float
    ((client client) (result-type (eql 'single-float)) (base (eql 10)) significand exponent sign)
  (%liebler client single-float
            significand exponent sign
            quaviver/math:expt10/32
            quaviver/math:round-to-odd/32))

(defmethod quaviver:integer-float
    ((client client) (result-type (eql 'double-float)) (base (eql 10)) significand exponent sign)
  (%liebler client double-float
            significand exponent sign
            quaviver/math:expt10/64
            quaviver/math:round-to-odd/64))

#+quaviver/long-float
(defmethod quaviver:integer-float
    ((client client) (result-type (eql 'long-float)) (base (eql 10)) significand exponent sign)
  (%liebler client long-float
            significand exponent sign
            quaviver/math:expt10/128
            quaviver/math:round-to-odd/128))
