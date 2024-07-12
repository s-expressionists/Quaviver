(in-package #:quaviver)

(defun integer-float-overflow (client result-type base significand exponent sign)
  (error 'floating-point-overflow
         :operation 'quaviver:integer-float
         :operands (list client result-type base significand exponent sign)))

(defun integer-float-underflow (client result-type base significand exponent sign)
  (error 'floating-point-underflow
         :operation 'quaviver:integer-float
         :operands (list client result-type base significand exponent sign)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod integer-float-form
      (client result-type (base (eql 2)) significand-form exponent-form sign-form)
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
        result-type
      (let ((bits-var (gensym))
            (significand-var (gensym))
            (exponent-var (gensym))
            (sign-var (gensym)))
        `(let ((,bits-var 0)
               (,significand-var ,significand-form)
               (,exponent-var ,exponent-form)
               (,sign-var ,sign-form))
           (declare (type (unsigned-byte ,storage-size)
                          ,bits-var ,significand-var)
                    (type (or fixnum keyword)
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
                 (t
                  (let ((shift (- ,significand-size
                                  (integer-length ,significand-var))))
                    (setf ,significand-var (ash ,significand-var shift))
                    (decf ,exponent-var shift))
                  (cond ((< ,exponent-var ,min-exponent)
                         (integer-float-underflow
                          ,client ',result-type 2 ,significand-var ,exponent-var ,sign-var))
                        ((> ,exponent-var ,max-exponent)
                         (integer-float-overflow
                          ,client ',result-type 2 ,significand-var ,exponent-var ,sign-var))
                        (t
                         (incf ,exponent-var ,exponent-bias)
                         (cond ((plusp ,exponent-var)
                                (setf (ldb ,significand-byte-form ,bits-var)
                                      ,significand-var
                                      (ldb ,exponent-byte-form ,bits-var)
                                      ,exponent-var))
                               (t        ; Unadjusted subnormal
                                (setf (ldb (byte (+ ,(byte-size significand-bytespec)
                                                    ,exponent-var)
                                                 ,(byte-position significand-bytespec))
                                           ,bits-var)
                                      (ldb (byte (+ ,(byte-size significand-bytespec)
                                                    ,exponent-var)
                                                 (- ,(1+ (byte-position significand-bytespec))
                                                    ,exponent-var))
                                           ,significand-var))))))))
           ,(quaviver:bits-float-form nil result-type bits-var))))))

(macrolet ((body (type significand exponent sign)
             (integer-float-form nil type 2 significand exponent sign)))

  (defmethod integer-float
      (client (result-type (eql 'single-float)) (base (eql 2)) significand exponent sign)
    (body single-float significand exponent sign))

  #+(or abcl allegro ccl clasp cmucl ecl lispworks mezzano sbcl)
  (defmethod integer-float
      (client (result-type (eql 'double-float)) (base (eql 2)) significand exponent sign)
    (body double-float significand exponent sign)))

#+quaviver/long-float
(defmethod integer-float
    (client (result-type (eql 'long-float)) (base (eql 2)) significand exponent sign)
  (%integer-encode-float client long-float
                         significand exponent sign))
