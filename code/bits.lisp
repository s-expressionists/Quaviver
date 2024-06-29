(in-package #:quaviver.bits)

#+(or abcl allegro ccl clasp cmucl ecl lispworks mezzano sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :quaviver.bits/single-float *features*)
  (pushnew :quaviver.bits/double-float *features*))

#+(and ecl long-float)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :quaviver.bits/long-float *features*)
  (handler-case
      (system:long-float-bits 0l0)
    (error (condition)
      (declare (ignore condition))
      (pushnew :quaviver.bits/long-float-fallback *features*))))

(declaim (inline short-float-bits
                 bits-short-float
                 single-float-bits
                 bits-single-float
                 double-float-bits
                 bits-double-float
                 long-float-bits
                 bits-long-float
                 ub32-sb32))

(defun ub32-sb32 (ub32)
  (if (logbitp 31 ub32)
      (- ub32 #.(ash 1 32))
      ub32))

(defun short-float-bits (value)
  (single-float-bits value))

(defun bits-short-float (value)
  (bits-single-float value))

(defun single-float-bits (value)
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
  (ldb (byte 32 0) (sb-kernel:single-float-bits value))
  #-(or abcl allegro ccl clasp cmucl ecl lispworks mezzano sbcl)
  (error "Unable to do single-float-bits."))

(defun bits-single-float (value)
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
  (sb-kernel:make-single-float (ub32-sb32 value))
  #-(or abcl allegro ccl clasp cmucl ecl lispworks mezzano sbcl)
  (error "Unable to do bits-single-float."))

(defun double-float-bits (value)
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
  (let ((upper (sb32-ub32 (kernel:double-float-high-bits value)))
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
    (logior (ash upper 32) lower))
  #-(or abcl allegro ccl clasp cmucl ecl lispworks mezzano sbcl)
  (error "Unable to do ouble-float-bits."))

(defun bits-double-float (value)
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
    (sb-kernel:make-double-float upper lower))
  #-(or abcl allegro ccl clasp cmucl ecl lispworks mezzano sbcl)
  (error "Unable to do bits-single-float."))

#+(and ecl quaviver.bits/long-float-fallback)
(ffi:def-union long-float/uint128
  (f :long-double)
  (u (:array :uint64-t 2)))

(defun long-float-bits (value)
  #-quaviver/long-float
  (double-float-bits value)
  #+(and quaviver/long-float
         ecl
         (not quaviver.bits/long-float-fallback))
  (system:long-float-bits value)
  #+(and ecl quaviver.bits/long-float-fallback)
  (ffi:with-foreign-object (v 'long-float/uint128)
    (setf (ffi:get-slot-value v 'long-float/uint128 'f) value)
    (let ((u (ffi:get-slot-value v 'long-float/uint128 'u)))
      (ldb (byte #+x86-64 80
                 #-x86-64 128
                 0)
           (logior (ffi:deref-array u '(:array :uint64-t 2) 0)
                   (ash (ffi:deref-array u '(:array :uint64-t 2) 1)
                        64))))))

(defun bits-long-float (value)
  #-quaviver/long-float
  (bits-double-float value)
  #+(and quaviver/long-float
         ecl
         (not quaviver.bits/long-float-fallback))
  (system:bits-long-float value)
  #+(and ecl quaviver.bits/long-float-fallback)
  (ffi:with-foreign-object (v 'long-float/uint128)
    (let ((u (ffi:get-slot-value v 'long-float/uint128 'u)))
      (setf (ffi:deref-array u '(:array :uint64-t 2) 0)
            (ldb (byte 64 0) value)
            (ffi:deref-array u '(:array :uint64-t 2) 1)
            (ldb (byte 64 64) value)
            (ffi:get-slot-value v 'long-float/uint128 'u)
            u))
    (ffi:get-slot-value v 'long-float/uint128 'f)))
