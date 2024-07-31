(in-package #:quaviver)

#+(and ecl long-float)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case
      (system:long-float-bits 0l0)
    (error (condition)
      (declare (ignore condition))
      (pushnew :quaviver/long-float-fallback *features*))))

(declaim (inline ub32-sb32))

(defun ub32-sb32 (ub32)
  (if (logbitp 31 ub32)
      (- ub32 #.(ash 1 32))
      ub32))

#+quaviver/short-float
(defmethod bits-float-form ((float-type (eql 'short-float)) value)
  #+clisp
  `(ffi:with-foreign-object (u '(ffi:c-union (value ffi:single-float)
                                             (bits  ffi:uint32)))
     (setf (ffi:slot (ffi:foreign-value u) 'bits) (ash ,value
                                                       ,(- (storage-size 'single-float)
                                                           (storage-size 'short-float))))
     (coerce (ffi:slot (ffi:foreign-value u) 'value) 'short-float)))

#-quaviver/short-float
(defmethod bits-float-form ((float-type (eql 'short-float)) value)
  (bits-float-form 'single-float value))

(defmethod bits-float-form ((float-type (eql 'single-float)) value)
  #+abcl
  `(system:make-single-float ,value)
  #+allegro
  (alexandria:with-gensyms
      (v)
    `(let ((,v ,value))
       (excl:shorts-to-single-float (ldb (byte 16 16) ,v)
                                    (ldb (byte 16 0) ,v))))
  #+ccl
  `(ccl::host-single-float-from-unsigned-byte-32 ,value)
  #+clasp
  `(ext:bits-to-single-float ,value)
  #+clisp
  `(ffi:with-foreign-object (u '(ffi:c-union (value ffi:single-float)
                                             (bits  ffi:uint32)))
     (setf (ffi:slot (ffi:foreign-value u) 'bits) ,value)
     (ffi:slot (ffi:foreign-value u) 'value))
  #+cmucl
  `(kernel:make-single-float (ub32-sb32 ,value))
  #+ecl
  `(system:bits-single-float ,value)
  #+lispworks
  (alexandria:with-gensyms
      (m)
    `(let ((,m (sys:make-typed-aref-vector 4)))
       (declare (optimize (speed 3) (float 0) (safety 0))
                (dynamic-extent ,m))
       (setf (sys:typed-aref '(unsigned-byte 32) ,m 0) ,value)
       (sys:typed-aref 'single-float ,m 0)))
  #+mezzano
  `(mezzano.extensions:ieee-binary32-to-single-float ,value)
  #+sbcl
  `(sb-kernel:make-single-float (ub32-sb32 ,value)))

(defmethod bits-float-form ((float-type (eql 'double-float)) value)
  #+abcl
  `(system:make-double-float ,value)
  #+allegro
  (alexandria:with-gensyms
      (v)
    `(let ((,v ,value))
       (excl:shorts-to-double-float (ldb (byte 16 48) ,v)
                                    (ldb (byte 16 32) ,v)
                                    (ldb (byte 16 16) ,v)
                                    (ldb (byte 16  0) ,v))))
  #+ccl
  (alexandria:with-gensyms
      (v)
    `(let ((,v ,value))
       (ccl::double-float-from-bits (ldb (byte 32 32) ,v)
                                    (ldb (byte 32  0) ,v))))
  #+clasp
  `(ext:bits-to-double-float ,value)
  #+clisp
  `(ffi:with-foreign-object (u '(ffi:c-union (value ffi:double-float)
                                             (bits  ffi:uint64)))
     (setf (ffi:slot (ffi:foreign-value u) 'bits) ,value)
     (ffi:slot (ffi:foreign-value u) 'value))
  #+cmucl
  (alexandria:with-gensyms
      (v)
    `(let ((,v ,value))
       (kernel:make-double-float (ub32-sb32 (ldb (byte 32 32) ,v))
                                 (ldb (byte 32 0) ,v))))
  #+ecl
  `(system:bits-double-float ,value)
  #+lispworks
  (alexandria:with-gensyms
      (m v)
    `(let ((,m (sys:make-typed-aref-vector 8))
           (,v ,value))
       (declare (optimize (speed 3) (float 0) (safety 0))
                (dynamic-extent ,m))
       #+little-endian
       (setf (sys:typed-aref '(unsigned-byte 32) ,m 0) (ldb (byte 32  0) ,v)
             (sys:typed-aref '(unsigned-byte 32) ,m 4) (ldb (byte 32 32) ,v))
       #-little-endian
       (setf (sys:typed-aref '(unsigned-byte 32) ,m 0) (ldb (byte 32 32) ,v)
             (sys:typed-aref '(unsigned-byte 32) ,m 4) (ldb (byte 32  0) ,v))
       (sys:typed-aref 'double-float ,m 0)))
  #+mezzano
  `(mezzano.extensions:ieee-binary64-to-double-float ,value)
  #+sbcl
  (alexandria:with-gensyms
      (v)
    `(let ((,v ,value))
       (sb-kernel:make-double-float (ub32-sb32 (ldb (byte 32 32) ,v))
                                    (ldb (byte 32 0) ,v)))))

#+(and ecl quaviver/long-float-fallback)
(ffi:def-union long-float/uint128
  (f :long-double)
  (u (:array :uint64-t 2)))

#+quaviver/long-float
(defmethod bits-float-form ((float-type (eql 'long-float)) value)
  #-quaviver/long-float-fallback
  `(system:bits-long-float ,value)
  #+quaviver/long-float-fallback
  (alexandria:with-gensyms
      (m n v)
    `(let ((,v ,value))
       (ffi:with-foreign-object (,m 'long-float/uint128)
         (let ((,n (ffi:get-slot-value ,m 'long-float/uint128 'u)))
           (setf (ffi:deref-array ,n '(:array :uint64-t 2) 0)
                 (ldb (byte 64 0) ,v)
                 (ffi:deref-array ,n '(:array :uint64-t 2) 1)
                 (ldb (byte 64 64) ,v)
                 (ffi:get-slot-value ,m 'long-float/uint128 'u)
                 ,n))
         (ffi:get-slot-value ,m 'long-float/uint128 'f)))))

#-quaviver/long-float
(defmethod bits-float-form ((float-type (eql 'long-float)) value)
  (bits-float-form 'double-float value))
