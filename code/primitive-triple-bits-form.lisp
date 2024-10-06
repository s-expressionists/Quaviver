(in-package #:quaviver)

(defmethod primitive-triple-bits-form (float-type significand exponent sign)
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
        (exponent-var significand-var bits-var)
      `(let ((,bits-var 0))
         (declare (type (unsigned-byte ,storage-size)
                        ,bits-var)
                  (type (or exponent-word keyword)
                        ,exponent)
                  (type fixnum ,sign)
                  (optimize speed))
         (when (minusp ,sign)
           (setf (ldb ,sign-byte-form ,bits-var) 1))
         (cond ((keywordp ,exponent)                
                (setf (ldb ,exponent-byte-form ,bits-var)
                      ,(1- (ash 1 (byte-size exponent-bytespec))))
                (ecase ,exponent
                  (:infinity)
                  (:quiet-nan
                   (setf (ldb ,nan-type-byte-form ,bits-var) 1
                         (ldb ,nan-payload-byte-form ,bits-var) ,significand))
                  (:signaling-nan
                   (setf (ldb ,nan-payload-byte-form ,bits-var)
                         (if (zerop ,significand) 1 ,significand)))))
               ((zerop ,significand))
               ((< ,exponent ,(- (expt 2 21)))
                (quaviver.condition:floating-point-underflow
                 ',float-type ,sign
                 'triple-float nil ',float-type 2
                 ,significand ,exponent ,sign))
               ((> ,exponent ,(1- (expt 2 21)))
                (quaviver.condition:floating-point-overflow
                 ',float-type ,sign
                 'triple-float nil ',float-type 2
                 ,significand ,exponent ,sign))
               (t
                (let* ((shift (- ,significand-size
                                (integer-length ,significand)))
                       (,significand-var (ash ,significand shift))
                       (,exponent-var (- ,exponent shift)))
                  (declare (type (unsigned-byte ,(+ significand-size 6))
                                 ,significand-var)
                           (type (or exponent-word keyword)
                                 ,exponent-var))
                  (cond ((< ,exponent-var ,min-exponent)
                         (quaviver.condition:floating-point-underflow
                          ',float-type ,sign
                          'triple-float nil ',float-type 2
                          ,significand-var ,exponent-var ,sign))
                        ((> ,exponent-var ,max-exponent)
                         (quaviver.condition:floating-point-overflow
                          ',float-type ,sign
                          'triple-float nil ',float-type 2
                          ,significand-var ,exponent-var ,sign))
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
                                           ,significand-var)))))))))
         ,bits-var))))
