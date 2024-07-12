(in-package #:quaviver)

#+quaviver/short-float
(defmethod float-bits-form ((float-type (eql 'short-float)) value)
  #+clisp
  `(ffi:with-foreign-object (u '(ffi:c-union (value ffi:single-float)
                                             (bits  ffi:uint32)))
     (setf (ffi:slot (ffi:foreign-value u) 'value) (coerce ,value 'single-float))
     (ash (ffi:slot (ffi:foreign-value u) 'bits)
          ,(- (storage-size 'short-float)
              (storage-size 'single-float)))))

(defmethod float-bits-form ((float-type (eql 'single-float)) value)
  #+abcl
  `(system:single-float-bits ,value)
  #+allegro
  (let ((us1 (gensym))
        (us0 (gensym)))
    `(multiple-value-bind (,us1 ,us0)
         (excl:single-float-to-shorts ,value)
       (logior (ash ,us1 16) ,us0)))
  #+ccl
  `(ccl::single-float-bits ,value)
  #+clasp
  `(ext:single-float-to-bits ,value)
  #+clisp
  `(ffi:with-foreign-object (u '(ffi:c-union (value ffi:single-float)
                                             (bits  ffi:uint32)))
     (setf (ffi:slot (ffi:foreign-value u) 'value) ,value)
     (ffi:slot (ffi:foreign-value u) 'bits))
  #+cmucl
  `(ldb (byte 32 0) (kernel:single-float-bits ,value))
  #+ecl
  `(system:single-float-bits ,value)
  #+lispworks
  (let ((m (gensym)))
    `(let ((,m (sys:make-typed-aref-vector 4)))
       (declare (optimize (speed 3) (float 0) (safety 0))
                (dynamic-extent ,m))
       (setf (sys:typed-aref 'single-float ,m 0) ,value)
       (sys:typed-aref '(unsigned-byte 32) ,m 0)))
  #+mezzano
  `(mezzano.extensions:single-float-to-ieee-binary32 ,value)
  #+sbcl
  `(ldb (byte 32 0) (sb-kernel:single-float-bits ,value)))

(defmethod float-bits-form ((float-type (eql 'double-float)) value)
  #+abcl
  (let ((v (gensym)))
    `(let ((,v ,value))
       (logior (ash (system:double-float-high-bits ,v) 32)
               (system:double-float-low-bits ,v))))
  #+allegro
  (let ((us3 (gensym))
        (us2 (gensym))
        (us1 (gensym))
        (us0 (gensym)))
    `(multiple-value-bind (,us3 ,us2 ,us1 ,us0)
         (excl:double-float-to-shorts ,value)
       (logior (ash ,us3 48) (ash ,us2 32) (ash ,us1 16) ,us0)))
  #+ccl
  (let ((upper (gensym))
        (lower (gensym)))
    `(multiple-value-bind (,upper ,lower)
         (ccl::double-float-bits ,value)
       (logior (ash ,upper 32) ,lower)))
  #+clasp
  `(ext:double-float-to-bits ,value)
  #+clisp
  `(ffi:with-foreign-object (u '(ffi:c-union (value ffi:double-float)
                                             (bits  ffi:uint64)))
     (setf (ffi:slot (ffi:foreign-value u) 'value) ,value)
     (ffi:slot (ffi:foreign-value u) 'bits))
  #+cmucl
  (let ((v (gensym)))
    `(let ((,v ,value))
       (logior (ash (ldb (byte 32 0) (kernel:double-float-high-bits ,v)) 32)
               (kernel:double-float-low-bits ,v))))
  #+ecl
  `(system:double-float-bits ,value)
  #+lispworks
  (let ((m (gensym)))
    `(let ((,m (sys:make-typed-aref-vector 8)))
       (declare (optimize (speed 3) (float 0) (safety 0))
                (dynamic-extent ,m))
       (setf (sys:typed-aref 'double-float ,m 0) ,value)
       #+little-endian
       (logior (ash (sys:typed-aref '(unsigned-byte 32) ,m 4) 32)
               (sys:typed-aref '(unsigned-byte 32) ,m 0))
       #-little-endian
       (logior (ash (sys:typed-aref '(unsigned-byte 32) ,m 0) 32)
               (sys:typed-aref '(unsigned-byte 32) ,m 4))))
  #+mezzano
  `(mezzano.extensions:double-float-to-ieee-binary64 ,value)
  #+sbcl
  (let ((v (gensym)))
    `(let ((,v ,value))
       (logior (ash (ldb (byte 32 0) (sb-kernel:double-float-high-bits ,v)) 32)
               (sb-kernel:double-float-low-bits ,v)))))

#+quaviver/long-float
(defmethod float-bits-form ((float-type (eql 'long-float)) value)
  #-quaviver/long-float-fallback
  `(system:long-float-bits ,value)
  #+quaviver/long-float-fallback
  (let ((m (gensym))
        (n (gensym)))
    `(ffi:with-foreign-object (,m 'long-float/uint128)
       (setf (ffi:get-slot-value ,m 'long-float/uint128 'f) ,value)
       (let ((,n (ffi:get-slot-value ,m 'long-float/uint128 'u)))
         (ldb (byte ,(quaviver:storage-size 'long-float) 0)
              (logior (ffi:deref-array ,n '(:array :uint64-t 2) 0)
                      (ash (ffi:deref-array ,n '(:array :uint64-t 2) 1)
                           64)))))))
