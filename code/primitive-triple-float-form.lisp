(in-package #:quaviver)

(declaim (inline #+quaviver/short-float
                 primitive-triple-float/short-float
                 primitive-triple-float/single-float
                 primitive-triple-float/double-float
                 #+quaviver/long-float
                 primitive-triple-float/long-float))

(defmacro %primitive-triple-float-form (float-type significand exponent sign)
  (with-accessors ((storage-size storage-size)
                   (significand-bytespec significand-bytespec)
                   (significand-byte-form significand-byte-form)
                   (exponent-bytespec exponent-bytespec)
                   (exponent-byte-form exponent-byte-form)
                   (sign-byte-form sign-byte-form)
                   (nan-payload-byte-form nan-payload-byte-form)
                   (nan-type-byte-form nan-type-byte-form)
                   (hidden-bit-p hidden-bit-p)
                   (exponent-bias exponent-bias)
                   (min-exponent min-exponent)
                   (max-exponent max-exponent)
                   (significand-size significand-size))
      float-type
    (alexandria:with-gensyms
        (exponent-var significand-var sign-var bits-var)
      `(let ((,bits-var 0)
             (,exponent-var ,exponent)
             (,significand-var ,significand)
             (,sign-var ,sign))
         (declare (type (unsigned-byte ,storage-size)
                        ,bits-var)
                  (type (unsigned-byte ,(+ significand-size 6))
                        ,significand-var)
                  (type (or exponent-word keyword)
                        ,exponent-var)
                  (type fixnum ,sign-var)
                  (optimize speed))
         (when (minusp ,sign-var)
           (setf (ldb ,sign-byte-form ,bits-var) 1))
         (cond ((keywordp ,exponent-var)
                (setf (ldb ,exponent-byte-form ,bits-var)
                      ,(1- (ash 1 (byte-size exponent-bytespec))))
                (ecase ,exponent-var
                  (:infinity)
                  (:quiet-nan
                   (setf (ldb ,nan-type-byte-form ,bits-var) 1
                         (ldb ,nan-payload-byte-form ,bits-var) ,significand-var))
                  (:signaling-nan
                   (setf (ldb ,nan-payload-byte-form ,bits-var)
                         (if (zerop ,significand-var) 1 ,significand-var)))))
               ((zerop ,significand-var))
               ((< ,exponent-var ,(- (expt 2 21)))
                (quaviver.condition:floating-point-underflow
                 ',float-type ,sign-var
                 'triple-float nil ',float-type 2
                 ,significand-var ,exponent-var ,sign-var))
               ((> ,exponent-var ,(1- (expt 2 21)))
                (quaviver.condition:floating-point-overflow
                 ',float-type ,sign-var
                 'triple-float nil ',float-type 2
                 ,significand-var ,exponent-var ,sign-var))
               (t
                (let ((shift (- ,significand-size
                                (integer-length ,significand-var))))
                  (setf ,significand-var (ash ,significand-var shift))
                  (decf ,exponent-var shift))
                (cond ((< ,exponent-var ,min-exponent)
                       (quaviver.condition:floating-point-underflow
                        ',float-type ,sign-var
                        'triple-float nil ',float-type 2
                        ,significand-var ,exponent-var ,sign-var))
                      ((> ,exponent-var ,max-exponent)
                       (quaviver.condition:floating-point-overflow
                        ',float-type ,sign-var
                        'triple-float nil ',float-type 2
                        ,significand-var ,exponent-var ,sign-var))
                      (t
                       (incf ,exponent-var ,exponent-bias)
                       (cond ((plusp ,exponent-var)
                              (setf (ldb ,significand-byte-form ,bits-var)
                                    ,significand-var
                                    (ldb ,exponent-byte-form ,bits-var)
                                    ,exponent-var))
                             (t ; Unadjusted subnormal
                              (setf (ldb (byte (+ ,(byte-size significand-bytespec)
                                                  ,exponent-var)
                                               ,(byte-position significand-bytespec))
                                         ,bits-var)
                                    (ldb (byte (+ ,(byte-size significand-bytespec)
                                                  ,exponent-var)
                                               (- ,(1+ (byte-position significand-bytespec))
                                                  ,exponent-var))
                                         ,significand-var))))))))
         ,(bits-float-form float-type bits-var)))))

#+quaviver/short-float
(defun primitive-triple-float/short-float (significand exponent sign)
  (%primitive-triple-float-form short-float significand exponent sign))

(defun primitive-triple-float/single-float (significand exponent sign)
  (%primitive-triple-float-form single-float significand exponent sign))

(defun primitive-triple-float/double-float (significand exponent sign)
  (%primitive-triple-float-form double-float significand exponent sign))

#+quaviver/long-float
(defun primitive-triple-float/long-float (significand exponent sign)
  (%primitive-triple-float-form long-float significand exponent sign))

#+quaviver/short-float
(defmethod primitive-triple-float-form ((float-type (eql 'short-float)) significand exponent sign)
  `(primitive-triple-float/short-float ,significand ,exponent ,sign))

#-quaviver/short-float
(defmethod primitive-triple-float-form ((float-type (eql 'short-float)) significand exponent sign)
  `(primitive-triple-float/single-float ,significand ,exponent ,sign))

(defmethod primitive-triple-float-form ((float-type (eql 'single-float)) significand exponent sign)
  `(primitive-triple-float/single-float ,significand ,exponent ,sign))

(defmethod primitive-triple-float-form ((float-type (eql 'double-float)) significand exponent sign)
  `(primitive-triple-float/double-float ,significand ,exponent ,sign))

#+quaviver/long-float
(defmethod primitive-triple-float-form ((float-type (eql 'long-float)) significand exponent sign)
  `(primitive-triple-float/long-float ,significand ,exponent ,sign))

#-quaviver/long-float
(defmethod primitive-triple-float-form ((float-type (eql 'long-float)) significand exponent sign)
  `(primitive-triple-float/double-float ,significand ,exponent ,sign))
