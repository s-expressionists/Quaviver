(in-package #:quaviver)

(defmacro %integer-encode-float (client type significand exponent sign)
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
      type
    (let ((exponent-var (gensym))
          (significand-var (gensym))
          (bits-var (gensym)))
      `(let ((,bits-var 0)
             (,exponent-var ,exponent)
             (,significand-var ,significand))
         (declare (type (unsigned-byte ,storage-size)
                        ,bits-var ,significand-var)
                  (type (or fixnum keyword)
                        ,exponent-var)
                  (type fixnum ,sign)
                  (optimize speed))
         (when (minusp ,sign)
           (setf (ldb ,sign-byte-form ,bits-var) 1))
         (cond ((keywordp exponent)
                (setf (ldb ,exponent-byte-form ,bits-var)
                      ,(1- (ash 1 (byte-size exponent-bytespec))))
                (ecase exponent
                  (:infinity)
                  (:quiet-nan
                   (setf (ldb ,nan-type-byte-form ,bits-var) 1
                         (ldb ,nan-payload-byte-form ,bits-var) ,significand-var))
                  (:signaling-nan
                   (setf (ldb ,nan-payload-byte-form ,bits-var)
                         (if (zerop ,significand-var) 1 ,significand-var)))))
               ((zerop ,significand-var))
               (t
                (let ((shift (- ,significand-size
                                (integer-length ,significand-var))))
                  (setf ,significand-var (ash ,significand-var shift))
                  (decf ,exponent-var shift))
                (cond ((< ,exponent-var ,min-exponent)
                       (error 'floating-point-underflow
                              :operation 'integer-float
                              :operands (list ,client ',type 2
                                              ,significand ,exponent ,sign)))
                      ((> ,exponent-var ,max-exponent)
                       (error 'floating-point-overflow
                              :operation 'integer-float
                              :operands (list ,client ',type 2
                                              ,significand ,exponent ,sign)))
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
         (quaviver:bits-float nil ',type ,bits-var)))))

(defmethod integer-float
    (client (result-type (eql 'single-float)) (base (eql 2)) significand exponent sign)
  (%integer-encode-float client single-float
                         significand exponent sign))

#+(or abcl allegro ccl clasp cmucl ecl lispworks mezzano sbcl)
(defmethod integer-float
    (client (result-type (eql 'double-float)) (base (eql 2)) significand exponent sign)
  (%integer-encode-float client double-float
                         significand exponent sign))

#+quaviver/long-float
(defmethod integer-float
    (client (result-type (eql 'long-float)) (base (eql 2)) significand exponent sign)
  (%integer-encode-float client long-float
                         significand exponent sign))
