(in-package #:quaviver)

(declaim (inline #+quaviver/short-float
                 float-internal-integer/short-float
                 float-internal-integer/single-float
                 float-internal-integer/double-float
                 #+quaviver/long-float
                 float-internal-integer/long-float))

(defmacro %float-internal-integer-form (float-type value)
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
       (cond ((= exponent ,(1- (ash 1 (byte-size exponent-bytespec))))
              (if (ldb-test ,significand-byte-form bits) ; nan
                  (values (ldb ,nan-payload-byte-form bits)
                          (if (ldb-test ,nan-type-byte-form bits)
                              :quiet-nan
                              :signaling-nan)
                          sign)
                  (values 0 :infinity sign)))
             (t
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
                       (values significand exponent sign)))))))))

#+quaviver/short-float
(defun float-internal-integer/short-float (value)
  (%float-internal-integer-form short-float value))

(defun float-internal-integer/single-float (value)
  (%float-internal-integer-form single-float value))

(defun float-internal-integer/double-float (value)
  (%float-internal-integer-form double-float value))

#+quaviver/long-float
(defun float-internal-integer/long-float (value)
  (%float-internal-integer-form long-float value))

#+quaviver/short-float
(defmethod float-internal-integer-form ((float-type (eql 'short-float)) value)
  `(float-internal-integer/short-float ,value))

#-quaviver/short-float
(defmethod float-internal-integer-form ((float-type (eql 'short-float)) value)
  `(float-internal-integer/single-float ,value))

(defmethod float-internal-integer-form ((float-type (eql 'single-float)) value)
  `(float-internal-integer/single-float ,value))

(defmethod float-internal-integer-form ((float-type (eql 'double-float)) value)
  `(float-internal-integer/double-float ,value))

#+quaviver/long-float
(defmethod float-internal-integer-form ((float-type (eql 'long-float)) value)
  `(float-internal-integer/long-float ,value))

#-quaviver/long-float
(defmethod float-internal-integer-form ((float-type (eql 'long-float)) value)
  `(float-internal-integer/double-float ,value))

(defmethod float-internal-integer-form (float-type value)
  (declare (ignore float-type))
  `(integer-decode-float ,value))
