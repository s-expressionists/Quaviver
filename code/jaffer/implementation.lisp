(in-package #:quaviver/jaffer)

(defclass client () ())

(defmacro %jaffer (client float-type significand exponent sign)
  (with-accessors ((significand-size quaviver:significand-size)
                   (min-exponent quaviver:min-exponent)
                   (max-exponent quaviver:max-exponent))
      float-type
    `(if (or (keywordp ,exponent)
             (zerop ,significand))
         ,(quaviver:primitive-triple-float-form float-type
                                                significand
                                                exponent
                                                sign)
         (let ((q (+ (quaviver.math:floor-log-expt 2 10 ,exponent)
                     (integer-length ,significand)
                     ,(- significand-size))))
           ;; The following overflow and underflow checks are not
           ;; strict checks. Stricter checks will happen in
           ;; triple-float/2. These are here to avoid excessible
           ;; large bignum in the intermediate calculations.
           (cond ((> q ,(+ max-exponent
                           (quaviver.math:ceiling-log-expt 2 10 1)))
                  (quaviver.condition:floating-point-overflow
                   'quaviver:triple-float
                   ,client ',float-type 10
                   ,significand ,exponent ,sign))
                 ((< q ,(- min-exponent
                           (quaviver.math:ceiling-log-expt 2 10 1)))
                  (quaviver.condition:floating-point-underflow
                   'quaviver:triple-float
                   ,client ',float-type 10
                   ,significand ,exponent ,sign))
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
                      ,(quaviver:primitive-triple-float-form float-type
                                                             'quotient
                                                             `(+ bex ,exponent)
                                                             sign))))
                 (t
                  (let* ((num (* ,significand (expt 5 ,exponent)))
                         (bex (- (integer-length num) ,significand-size)))
                    (if (plusp bex)
                        ,(quaviver:primitive-triple-float-form float-type
                                                               `(round num (ash 1 bex))
                                                               `(+ bex ,exponent)
                                                               sign)
                        ,(quaviver:primitive-triple-float-form float-type
                                                               'num
                                                               exponent
                                                               sign)))))))))

#+quaviver/short-float
(defmethod quaviver:triple-float
    ((client client) (float-type (eql 'short-float)) (base (eql 10))
     significand exponent sign)
  (%jaffer client short-float significand exponent sign))

(defmethod quaviver:triple-float
    ((client client) (float-type (eql 'single-float)) (base (eql 10))
     significand exponent sign)
  (%jaffer client single-float significand exponent sign))

(defmethod quaviver:triple-float
    ((client client) (float-type (eql 'double-float)) (base (eql 10))
     significand exponent sign)
  (%jaffer client double-float significand exponent sign))

#+quaviver/long-float
(defmethod quaviver:triple-float
    ((client client) (float-type (eql 'long-float)) (base (eql 10))
     significand exponent sign)
  (%jaffer client long-float significand exponent sign))
