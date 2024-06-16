(in-package #:quaviver)

(defmacro %integer-decode-float
    (value
     &key significand-size
          exponent-size
          ((:hidden-bit hidden-bit-p) nil)
           exponent-bias)
  `(let* ((bits ,value)
          (significand (ldb (byte ,significand-size 0) bits))
          (exponent (ldb (byte ,exponent-size ,significand-size) bits))
          (sign (if (logbitp ,(+ exponent-size significand-size) bits) -1 1)))
     (declare (type (unsigned-byte ,(+ 1 exponent-size significand-size))
                    bits)
              (type (unsigned-byte ,significand-size)
                    significand)
              (type (unsigned-byte ,exponent-size)
                    exponent)
              (type (integer -1 1)
                    sign))
     (if (zerop exponent) ; subnormal
         (let ((shift (- ,(if hidden-bit-p (1+ significand-size) significand-size)
                         (integer-length significand))))
           (values (ash significand shift)
                   (- ,(- 1 exponent-bias) shift)
                   sign))
         (values ,(if hidden-bit-p
                      `(logior significand ,(ash 1 significand-size))
                      'significand)
                 (- exponent ,exponent-bias)
                 sign))))

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
   #+abcl      (system:single-float-bits value)
   #+allegro   (multiple-value-bind (us1 us0)
                   (excl:single-float-to-shorts value)
                 (logior (ash us1 16) us0))
   #+clasp     (ext:single-float-to-bits value)
   #+cmucl     (ldb (byte 32 0)
                    (kernel:single-float-bits value))
   #+ecl       (system:single-float-to-bits value)
   #+lispworks (let ((v (sys:make-typed-aref-vector 4)))
                 (declare (optimize (speed 3) (float 0) (safety 0))
                          (dynamic-extent v))
                 (setf (sys:typed-aref 'single-float v 0) value)
                 (sys:typed-aref '(unsigned-byte 32) v 0))
   #+sbcl      (ldb (byte 32 0)
                    (sb-kernel:single-float-bits value))
   :significand-size 23
   :exponent-size 8
   :hidden-bit t
   :exponent-bias 150))

#+(or abcl allegro clasp cmucl ecl lispworks sbcl)
(defmethod float-integer (client (base (eql 2)) (value double-float))
  (declare (ignore client))
  (%integer-decode-float
   #+abcl      (logior (ash (system:double-float-high-bits value) 32)
                       (system:double-float-low-bits value))
   #+allegro   (multiple-value-bind (us3 us2 us1 us0)
                   (excl:double-float-to-shorts value)
                 (logior (ash us3 48) (ash us2 32) (ash us1 16) us0))
   #+clasp     (ext:double-float-to-bits value)
   #+cmucl     (logior (ash (ldb (byte 32 0)
                                 (kernel:double-float-high-bits value))
                            32)
                       (ldb (byte 32 0)
                            (kernel:double-float-low-bits value)))
   #+ecl       (system:double-float-to-bits value)
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
                       (ldb (byte 32 0)
                            (sb-kernel:double-float-low-bits value)))
   :significand-size 52
   :exponent-size 11
   :hidden-bit t
   :exponent-bias 1075))
