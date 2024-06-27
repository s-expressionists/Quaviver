(in-package #:quaviver/jaffer)

(defclass client () ())

(defmacro %jaffer (client result-type significand exponent sign)
  (let ((significand-size (quaviver:significand-size result-type)))
    `(if (or (not (numberp ,exponent))
             (zerop ,significand))
         (quaviver:integer-float ,client ',result-type 2
                                 ,significand ,exponent ,sign)
         (* sign
            (if (minusp ,exponent)
                (let* ((scale (expt 5 (- ,exponent)))
                       (bex (- (integer-length ,significand)
                               (integer-length scale)
                               ,significand-size))
                       (tmp (+ bex ,exponent 1021 ,significand-size))
                       (mantlen ,significand-size))
                  (when (minusp tmp)
                    (decf bex (1+ tmp))
                    (incf mantlen tmp))
                  (let* ((num (ash ,significand (- bex)))
                         (quotient (round num scale)))
                    (when (> (integer-length quotient) mantlen)
                      (incf bex)
                      (setf quotient (round num (ash scale 1))))
                    (scale-float (coerce quotient ',result-type) (+ bex exponent))))
                (let* ((num (* ,significand (expt 5 ,exponent)))
                       (bex (- (integer-length num) ,significand-size)))
                  (if (plusp bex)
                      (scale-float (coerce (round num (ash 1 bex)) ',result-type)
                                   (+ bex exponent))
                      (scale-float (coerce num ',result-type) ,exponent))))))))

(defmethod quaviver:integer-float
    ((client client) (result-type (eql 'single-float)) (base (eql 10)) significand exponent sign)
  (%jaffer client single-float
           significand exponent sign))

(defmethod quaviver:integer-float
    ((client client) (result-type (eql 'double-float)) (base (eql 10)) significand exponent sign)
  (%jaffer client double-float
           significand exponent sign))

#+quaviver/long-float
(defmethod quaviver:integer-float
    ((client client) (result-type (eql 'long-float)) (base (eql 10)) significand exponent sign)
  (%jaffer client long-float
           significand exponent sign))
