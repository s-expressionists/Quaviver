(in-package #:quaviver)

#+(or abcl allegro ccl clasp cmucl ecl lispworks mezzano sbcl)
(defmethod float-bits (client (value single-float))
  (declare (ignore client))
  #+abcl
  (system:single-float-bits value)
  #+allegro
  (multiple-value-bind (us1 us0)
      (excl:single-float-to-shorts value)
    (logior (ash us1 16) us0))
  #+ccl
  (ccl::single-float-bits value)
  #+clasp
  (ext:single-float-to-bits value)
  #+cmucl
  (ldb (byte 32 0) (kernel:single-float-bits value))
  #+ecl
  (system:single-float-bits value)
  #+lispworks
  (let ((v (sys:make-typed-aref-vector 4)))
    (declare (optimize (speed 3) (float 0) (safety 0))
             (dynamic-extent v))
    (setf (sys:typed-aref 'single-float v 0) value)
    (sys:typed-aref '(unsigned-byte 32) v 0))
  #+mezzano
  (mezzano.extensions:single-float-to-ieee-binary32 value)
  #+sbcl
  (ldb (byte 32 0) (sb-kernel:single-float-bits value)))

#+(or abcl allegro ccl clasp cmucl ecl lispworks mezzano sbcl)
(defmethod float-bits (client (value double-float))
  (declare (ignore client))
  #+abcl
  (let ((upper (system:double-float-high-bits value))
        (lower (system:double-float-low-bits value)))
    (logior (ash upper 32) lower))
  #+allegro
  (multiple-value-bind (us3 us2 us1 us0)
      (excl:double-float-to-shorts value)
    (logior (ash us3 48) (ash us2 32) (ash us1 16) us0))
  #+ccl
  (multiple-value-bind (upper lower)
      (ccl::double-float-bits value)
    (logior (ash upper 32) lower))
  #+clasp
  (ext:double-float-to-bits value)
  #+cmucl
  (let ((upper (ldb (byte 32 0) (kernel:double-float-high-bits value)))
        (lower (kernel:double-float-low-bits value)))
    (logior (ash upper 32) lower))
  #+ecl
  (system:double-float-bits value)
  #+lispworks
  (let ((v (sys:make-typed-aref-vector 8)))
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
  #+mezzano
  (mezzano.extensions:double-float-to-ieee-binary64 value)
  #+sbcl
  (let ((upper (ldb (byte 32 0) (sb-kernel:double-float-high-bits value)))
        (lower (sb-kernel:double-float-low-bits value)))
    (logior (ash upper 32) lower)))

#+quaviver/long-float
(defmethod float-bits (client (value long-float))
  (declare (ignore client))
  #-quaviver/long-float-fallback
  (system:long-float-bits value)
  #+quaviver/long-float-fallback
  (ffi:with-foreign-object (v 'long-float/uint128)
    (setf (ffi:get-slot-value v 'long-float/uint128 'f) value)
    (let ((u (ffi:get-slot-value v 'long-float/uint128 'u)))
      (ldb (byte #+x86-64 80
                 #-x86-64 128
                 0)
           (logior (ffi:deref-array u '(:array :uint64-t 2) 0)
                   (ash (ffi:deref-array u '(:array :uint64-t 2) 1)
                        64))))))

