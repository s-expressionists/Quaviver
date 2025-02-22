(in-package #:quaviver)

(defmethod bits-primitive-triple-form (float-type bits-var)
  (with-accessors ((storage-size storage-size)
                   (significand-size significand-size)
                   (significand-bytespec significand-bytespec)
                   (significand-byte-form significand-byte-form)
                   (exponent-bytespec exponent-bytespec)
                   (exponent-byte-form exponent-byte-form)
                   (sign-byte-form sign-byte-form)
                   (nan-payload-byte-form nan-payload-byte-form)
                   (nan-type-byte-form nan-type-byte-form)
                   (hidden-bit-p hidden-bit-p)
                   (exponent-bias exponent-bias)
                   (arithmetic-size arithmetic-size))
      float-type
    `(let ((exponent (ldb ,exponent-byte-form ,bits-var))
           (sign (if (ldb-test ,sign-byte-form ,bits-var) -1 1)))
       (declare (type (unsigned-byte ,storage-size) ,bits-var)
                (type exponent-word exponent)
                (type fixnum sign))
       (cond ((= exponent ,(1- (ash 1 (byte-size exponent-bytespec))))
              (if (ldb-test ,significand-byte-form ,bits-var) ; nan
                  (values (ldb ,nan-payload-byte-form ,bits-var)
                          (if (ldb-test ,nan-type-byte-form ,bits-var)
                              :quiet-nan
                              :signaling-nan)
                          sign)
                  (values 0 :infinity sign)))
             (t
              (let ((significand (ldb ,significand-byte-form ,bits-var)))
                (declare (type (unsigned-byte ,(+ 6 significand-size))
                               significand))
                (cond ((and (zerop significand)
                            (zerop exponent))
                       (values 0 0 sign))
                      (t
                       (if (zerop exponent) ; subnormal
                           (let ((shift (- ,significand-size
                                           (integer-length significand))))
                             (setf significand (ash significand shift)
                                   exponent (- ,(- 1 exponent-bias) shift)))
                           (setf ,@(when hidden-bit-p
                                     `(significand (logior significand
                                                           ,(ash 1 (byte-size significand-bytespec)))))
                                 exponent (- exponent ,exponent-bias)))
                       (values significand exponent sign)))))))))
