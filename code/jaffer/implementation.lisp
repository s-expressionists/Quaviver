(in-package #:quaviver/jaffer)

(defclass client () ())

(defmacro %jaffer (client result-type significand exponent sign)
  (with-accessors ((significand-size quaviver:significand-size)
                   (min-exponent quaviver:min-exponent)
                   (max-exponent quaviver:max-exponent))
      result-type
    `(if (or (not (numberp ,exponent))
             (zerop ,significand))
         (quaviver:integer-float ,client ',result-type 2
                                 ,significand ,exponent ,sign)
         (let ((q (+ (quaviver.math:floor-log-expt 2 10 ,exponent)
                     (integer-length ,significand)
                     ,(- significand-size))))
           ;; The following overflow and underflow checks are not
           ;; strict checks. Stricter checks will happen in
           ;; integer-float/2. These are here to avoid excessible
           ;; large bignum in the intermediate calculations.
           (cond ((> q ,(+ max-exponent
                          (quaviver.math:ceiling-log-expt 2 10 1)))
                  (error 'floating-point-overflow
                         :operation 'quaviver:integer-float
                         :operands (list ,client ',result-type 10
                                         ,significand ,exponent ,sign)))
                 ((< q ,(- min-exponent
                           (quaviver.math:ceiling-log-expt 2 10 1)))
                  (error 'floating-point-underflow
                         :operation 'quaviver:integer-float
                         :operands (list ,client ',result-type 10
                                         ,significand ,exponent ,sign)))
                 ((minusp ,exponent)
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
                      (quaviver:integer-float ,client ',result-type 2
                                              quotient
                                              (+ bex exponent)
                                              ,sign))))
                 (t
                  (let* ((num (* ,significand (expt 5 ,exponent)))
                         (bex (- (integer-length num) ,significand-size)))
                    (if (plusp bex)
                        (quaviver:integer-float ,client ',result-type 2
                                                (round num (ash 1 bex))
                                                (+ bex exponent)
                                                ,sign)
                        (quaviver:integer-float ,client ',result-type 2
                                                num
                                                ,exponent
                                                ,sign)))))))))

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
