(in-package #:quaviver/liebler)

(defclass client ()
  ())

(defmacro %liebler (client float-type significand-var exponent-var sign-var)
  (with-accessors ((arithmetic-size quaviver:arithmetic-size)
                   (significand-size quaviver:significand-size)
                   (min-exponent quaviver:min-exponent)
                   (max-exponent quaviver:max-exponent))
      float-type
    (let* ((extra 6)
           (word-size (+ extra significand-size)))
      `(locally
           (declare #+(or)(type (unsigned-byte ,word-size)
                          ,significand-var)
                    (type (or quaviver:exponent-word keyword) ,exponent-var)
                    (type fixnum ,sign-var)
                    (optimize speed))
         (if (or (keywordp ,exponent-var)
                 (zerop ,significand-var))
             ,(quaviver:primitive-triple-float-form float-type significand-var exponent-var sign-var)
             (let* ((shift (- ,significand-size (integer-length ,significand-var)))
                    (k (- (quaviver.math:floor-log-expt 2 10 ,exponent-var) -1 shift)))
               (declare (type quaviver:exponent-word k shift))
               ;; The following overflow and underflow checks are not
               ;; strict checks. Stricter checks will happen in
               ;; triple-float/2. These are here to protect the expt10
               ;; table lookup from an out of bounds error.
               (cond ((> k ,(+ max-exponent
                               (quaviver.math:ceiling-log-expt 2 10 1)))
                      (quaviver.condition:floating-point-overflow
                       'quaviver:triple-float
                       ,client ',float-type 10 ,significand-var ,exponent-var ,sign-var))
                     ((< k ,(- min-exponent
                               (quaviver.math:ceiling-log-expt 2 10 1)))
                      (quaviver.condition:floating-point-underflow
                       'quaviver:triple-float
                       ,client ',float-type 10 ,significand-var ,exponent-var ,sign-var))
                     (t
                      (setf ,significand-var (quaviver.math:round-to-odd
                                              ,arithmetic-size
                                              (ash ,significand-var (+ shift ,extra))
                                              (quaviver.math:expt ,arithmetic-size 10
                                                                  (- ,exponent-var)))
                            shift (- ,significand-size (integer-length ,significand-var)))
                      ,(quaviver:primitive-triple-float-form float-type
                                                             `(round ,significand-var (ash 1 (- shift)))
                                                             `(- k shift ,extra)
                                                             sign-var)))))))))

#+quaviver/short-float
(defmethod quaviver:triple-float
    ((client client) (float-type (eql 'short-float)) (base (eql 10))
     significand exponent sign)
  (%liebler client short-float significand exponent sign))

(defmethod quaviver:triple-float
    ((client client) (float-type (eql 'single-float)) (base (eql 10))
     significand exponent sign)
  (%liebler client single-float significand exponent sign))

(defmethod quaviver:triple-float
    ((client client) (float-type (eql 'double-float)) (base (eql 10))
     significand exponent sign)
  (%liebler client double-float significand exponent sign))

#+quaviver/long-float
(defmethod quaviver:triple-float
    ((client client) (float-type (eql 'long-float)) (base (eql 10))
     significand exponent sign)
  (%liebler client long-float significand exponent sign))
