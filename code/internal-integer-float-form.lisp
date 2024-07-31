(in-package #:quaviver)

(declaim (inline #+quaviver/short-float
                 internal-integer-float/short-float
                 internal-integer-float/single-float
                 internal-integer-float/double-float
                 #+quaviver/long-float
                 internal-integer-float/long-float))

(defmacro %internal-integer-float-form (float-type significand exponent sign)
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
               (t
                (let ((shift (- ,significand-size
                                (integer-length ,significand-var))))
                  (setf ,significand-var (ash ,significand-var shift))
                  (decf ,exponent-var shift))
                (cond ((< ,exponent-var ,min-exponent)
                       (quaviver.condition:floating-point-underflow
                        'integer-float
                        ,significand-var ,exponent-var ,sign-var))
                      ((> ,exponent-var ,max-exponent)
                       (quaviver.condition:floating-point-overflow
                        'integer-float
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
(defun internal-integer-float/short-float (significand exponent sign)
  (%internal-integer-float-form short-float significand exponent sign))

(defun internal-integer-float/single-float (significand exponent sign)
  (%internal-integer-float-form single-float significand exponent sign))

(defun internal-integer-float/double-float (significand exponent sign)
  (%internal-integer-float-form double-float significand exponent sign))

#+quaviver/long-float
(defun internal-integer-float/long-float (significand exponent sign)
  (%internal-integer-float-form long-float significand exponent sign))

#+quaviver/short-float
(defmethod internal-integer-float-form ((float-type (eql 'short-float)) significand exponent sign)
  `(internal-integer-float/short-float ,significand ,exponent ,sign))

#-quaviver/short-float
(defmethod internal-integer-float-form ((float-type (eql 'short-float)) significand exponent sign)
  `(internal-integer-float/single-float ,significand ,exponent ,sign))

(defmethod internal-integer-float-form ((float-type (eql 'single-float)) significand exponent sign)
  `(internal-integer-float/single-float ,significand ,exponent ,sign))

(defmethod internal-integer-float-form ((float-type (eql 'double-float)) significand exponent sign)
  `(internal-integer-float/double-float ,significand ,exponent ,sign))

#+quaviver/long-float
(defmethod internal-integer-float-form ((float-type (eql 'long-float)) significand exponent sign)
  `(internal-integer-float/long-float ,significand ,exponent ,sign))

#-quaviver/long-float
(defmethod internal-integer-float-form ((float-type (eql 'long-float)) significand exponent sign)
  `(internal-integer-float/double-float ,significand ,exponent ,sign))
