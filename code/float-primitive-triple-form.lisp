(in-package #:quaviver)

(declaim (inline float-primitive-triple/short-float
                 float-primitive-triple/single-float
                 float-primitive-triple/double-float
                 float-primitive-triple/long-float))

(defmacro %float-primitive-triple-form (float-type value)
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
    `(let* ((bits ,(float-bits-form float-type value))
            (exponent (ldb ,exponent-byte-form bits))
            (sign (if (ldb-test ,sign-byte-form bits) -1 1)))
       (declare (type (unsigned-byte ,storage-size) bits)
                (type exponent-word exponent)
                (type fixnum sign))
       (cond ((/= exponent ,(1- (ash 1 (byte-size exponent-bytespec))))
              (let ((significand (ldb ,significand-byte-form bits)))
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
                       (values significand exponent sign)))))
             ((ldb-test ,nan-type-byte-form bits)
              (values (ldb ,nan-payload-byte-form bits)
                      :quiet-nan
                      sign))
             ((ldb-test ,nan-payload-byte-form bits)
              (values (ldb ,nan-payload-byte-form bits)
                      :signaling-nan
                      sign))
             (t
              (values 0 :infinity sign))))))

(defun float-primitive-triple/short-float (value)
  (%float-primitive-triple-form #+quaviver/short-float short-float
                                #-quaviver/short-float single-float
                                value))

(defun float-primitive-triple/single-float (value)
  (%float-primitive-triple-form single-float value))

(defun float-primitive-triple/double-float (value)
  (%float-primitive-triple-form double-float value))

(defun float-primitive-triple/long-float (value)
  (%float-primitive-triple-form #+quaviver/long-float long-float
                                #-quaviver/long-float double-float
                                value))

(defmethod float-primitive-triple-form ((float-type (eql 'short-float)) value)
  `(float-primitive-triple/short-float ,value))

(defmethod float-primitive-triple-form ((float-type (eql 'single-float)) value)
  `(float-primitive-triple/single-float ,value))

(defmethod float-primitive-triple-form ((float-type (eql 'double-float)) value)
  `(float-primitive-triple/double-float ,value))

(defmethod float-primitive-triple-form ((float-type (eql 'long-float)) value)
  `(float-primitive-triple/long-float ,value))

(defmethod float-primitive-triple-form (float-type value)
  (declare (ignore float-type))
  `(integer-decode-float ,value))
