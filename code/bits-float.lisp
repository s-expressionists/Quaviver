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

#+(or abcl allegro ccl clasp cmucl ecl lispworks mezzano sbcl)
(defmethod bits-float (client (result-type (eql 'single-float)) value)
  (declare (ignore client))
  #+abcl
  (system:make-single-float value)
  #+allegro
  (excl:shorts-to-single-float (ldb (byte 16 16) value)
                               (ldb (byte 16 0) value))
  #+ccl
  (ccl::host-single-float-from-unsigned-byte-32 value)
  #+clasp
  (ext:bits-to-single-float value)
  #+cmucl
  (kernel:make-single-float (ub32-sb32 value))
  #+ecl
  (system:bits-single-float value)
  #+lispworks
  (let ((v (sys:make-typed-aref-vector 4)))
    (declare (optimize (speed 3) (float 0) (safety 0))
             (dynamic-extent v))
    (setf (sys:typed-aref '(unsigned-byte 32) v 0) value)
    (sys:typed-aref 'single-float v 0))
  #+mezzano
  (mezzano.extensions:ieee-binary32-to-single-float value)
  #+sbcl
  (sb-kernel:make-single-float (ub32-sb32 value)))

#+(or abcl allegro ccl clasp cmucl ecl lispworks mezzano sbcl)
(defmethod bits-float (client (result-type (eql 'double-float)) value)
  (declare (ignore client))
  #+abcl
  (system:make-double-float value)
  #+allegro
  (let ((us3 (ldb (byte 16 48) value))
        (us2 (ldb (byte 16 32) value))
        (us1 (ldb (byte 16 16) value))
        (us0 (ldb (byte 16 0) value)))
    (excl:shorts-to-double-float us3 us2 us1 us0))
  #+ccl
  (let ((upper (ldb (byte 32 32) value))
        (lower (ldb (byte 32 0) value)))
    (ccl::double-float-from-value upper lower))
  #+clasp
  (ext:bits-to-double-float value)
  #+cmu
  (let ((upper (ub32-sb32 (ldb (byte 32 32) value)))
        (lower (ldb (byte 32 0) value)))
    (kernel:make-double-float upper lower))
  #+ecl
  (system:bits-double-float value)
  #+lispworks
  (let ((upper (ldb (byte 32 32) value))
        (lower (ldb (byte 32 0) value))
        (v (sys:make-typed-aref-vector 8)))
    (declare (optimize (speed 3) (float 0) (safety 0))
             (dynamic-extent v))
    #+little-endian
    (setf (sys:typed-aref '(unsigned-byte 32) v 0) lower
          (sys:typed-aref '(unsigned-byte 32) v 4) upper)
    #-little-endian
    (setf (sys:typed-aref '(unsigned-byte 32) v 0) upper
          (sys:typed-aref '(unsigned-byte 32) v 4) lower)
    (sys:typed-aref 'double-float v 0))
  #+mezzano
  (mezzano.extensions:ieee-binary64-to-double-float value)
  #+sbcl
  (let ((upper (ub32-sb32 (ldb (byte 32 32) value)))
        (lower (ldb (byte 32 0) value)))
    (sb-kernel:make-double-float upper lower)))

#+(and ecl quaviver/long-float-fallback)
(ffi:def-union long-float/uint128
  (f :long-double)
  (u (:array :uint64-t 2)))

#+quaviver/long-float
(defmethod bits-float (client (result-type (eql 'long-float)) value)
  (declare (ignore client))
  #-quaviver/long-float-fallback
  (system:bits-long-float value)
  #+quaviver/long-float-fallback
  (ffi:with-foreign-object (v 'long-float/uint128)
    (let ((u (ffi:get-slot-value v 'long-float/uint128 'u)))
      (setf (ffi:deref-array u '(:array :uint64-t 2) 0)
            (ldb (byte 64 0) value)
            (ffi:deref-array u '(:array :uint64-t 2) 1)
            (ldb (byte 64 64) value)
            (ffi:get-slot-value v 'long-float/uint128 'u)
            u))
    (ffi:get-slot-value v 'long-float/uint128 'f)))
