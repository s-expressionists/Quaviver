(in-package #:quaviver)

(defmacro %integer-decode-float (type value)
  (with-accessors ((storage-size storage-size)
                   (significand-size significand-size)
                   (significand-bytespec significand-bytespec)
                   (exponent-bytespec exponent-bytespec)
                   (sign-bytespec sign-bytespec)
                   (nan-payload-bytespec nan-payload-bytespec)
                   (nan-type-bytespec nan-type-bytespec)
                   (hidden-bit-p hidden-bit-p)
                   (exponent-bias exponent-bias))
      type
    `(let* ((bits ,value)
            (exponent (ldb ',exponent-bytespec bits))
            (sign (if (ldb-test ',sign-bytespec bits) -1 1)))
       (declare (type (unsigned-byte ,storage-size)
                      bits)
                (type (unsigned-byte ,(byte-size exponent-bytespec))
                      exponent)
                (type (integer -1 1)
                      sign))
       (cond ((= exponent ,(1- (ash 1 (byte-size exponent-bytespec))))
              (if (ldb-test ',significand-bytespec bits) ; nan
                  (values (ldb ',nan-payload-bytespec bits)
                          (if (ldb-test ',nan-type-bytespec bits)
                              :quiet-nan
                              :signaling-nan)
                          1)
                  (values 0 :infinity sign)))
             (t
              (let ((significand (ldb ',significand-bytespec bits)))
                (declare (type (unsigned-byte ,(byte-size significand-bytespec))
                               significand))
                (cond ((and (zerop significand)
                            (zerop exponent))
                       (values 0 0 sign))
                      ((zerop exponent) ; subnormal
                       (let ((shift (- ,significand-size
                                       (integer-length significand))))
                         (values (ash significand shift)
                                 (- ,(- 1 exponent-bias) shift)
                                 sign)))
                      (t
                       (values ,(if hidden-bit-p
                                    `(logior significand ,(ash 1 (byte-size significand-bytespec)))
                                    'significand)
                               (- exponent ,exponent-bias)
                               sign)))))))))

#-(or abcl allegro clasp cmucl ecl lispworks sbcl)
(defmethod float-integer (client (base (eql 2)) value)
  (declare (ignore client))
  ;; CCL has good behavior so use integer-decode-float. Fallback to
  ;; integer-decode-float for unknown CL implementations.
  (integer-decode-float value))

#+(or abcl allegro clasp cmucl ecl lispworks sbcl)
(defmethod float-integer (client (base (eql 2)) (value single-float))
  (declare (ignore client))
  (%integer-decode-float
    single-float
   #+abcl      (system:single-float-bits value)
   #+allegro   (multiple-value-bind (us1 us0)
                   (excl:single-float-to-shorts value)
                 (logior (ash us1 16) us0))
   #+clasp     (ext:single-float-to-bits value)
   #+cmucl     (ldb (byte 32 0)
                    (kernel:single-float-bits value))
   #+ecl       (system:single-float-bits value)
   #+lispworks (let ((v (sys:make-typed-aref-vector 4)))
                 (declare (optimize (speed 3) (float 0) (safety 0))
                          (dynamic-extent v))
                 (setf (sys:typed-aref 'single-float v 0) value)
                 (sys:typed-aref '(unsigned-byte 32) v 0))
   #+sbcl      (ldb (byte 32 0)
                    (sb-kernel:single-float-bits value))))

#+(or abcl allegro clasp cmucl ecl lispworks sbcl)
(defmethod float-integer (client (base (eql 2)) (value double-float))
  (declare (ignore client))
  (%integer-decode-float
   double-float
   #+abcl      (logior (ash (system:double-float-high-bits value) 32)
                       (system:double-float-low-bits value))
   #+allegro   (multiple-value-bind (us3 us2 us1 us0)
                   (excl:double-float-to-shorts value)
                 (logior (ash us3 48) (ash us2 32) (ash us1 16) us0))
   #+clasp     (ext:double-float-to-bits value)
   #+cmucl     (logior (ash (ldb (byte 32 0)
                                 (kernel:double-float-high-bits value))
                            32)
                       (kernel:double-float-low-bits value))
   #+ecl       (system:double-float-bits value)
   #+lispworks (let ((v (sys:make-typed-aref-vector 8)))
                 (declare (optimize (speed 3) (float 0) (safety 0))
                          (dynamic-extent v))
                 (setf (sys:typed-aref 'double-float v 0) value)
                 #+little-endian
                 (let ((upper (sys:typed-aref '(unsigned-byte 32) v 4))
                       (lower (sys:typed-aref '(unsigned-byte 32) v 0)))
                   (logior (ash upper 32) lower))
                 #-little-endian
                 (let ((upper (sys:typed-aref '(unsigned-byte 32) v 0))
                       (lower (sys:typed-aref '(unsigned-byte 32) v 4)))
                   (logior (ash upper 32) lower)))
   #+sbcl      (logior (ash (ldb (byte 32 0)
                                 (sb-kernel:double-float-high-bits value))
                            32)
                       (sb-kernel:double-float-low-bits value))))

#+quaviver/long-float
(defmethod float-integer (client (base (eql 2)) (value long-float))
  (declare (ignore client))
  (%integer-decode-float long-float
                         (system:long-float-bits value)))
