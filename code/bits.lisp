(in-package #:quaviver.bits)

#+(and ecl quaviver.bits/long-float)
(ffi:def-union long-float/uint128
  (f :long-double)
  (u (:array :uint64-t 2)))

#+(and ecl quaviver.bits/long-float)
(defun long-float-bits (float)
  (ffi:with-foreign-object (v 'long-float/uint128)
    (setf (ffi:get-slot-value v 'long-float/uint128 'f) float)
    (let ((u (ffi:get-slot-value v 'long-float/uint128 'u)))
      (ldb (byte #+x86-64 80
                 #-x86-64 128
                 0)
           (logior (ffi:deref-array u '(:array :uint64-t 2) 0)
                   (ash (ffi:deref-array u '(:array :uint64-t 2) 1)
                        64))))))

(defun bits-long-float (integer)
  #+(and ecl quaviver.bits/long-float)
  (ffi:with-foreign-object (v 'long-float/uint128)
    (let ((u (ffi:get-slot-value v 'long-float/uint128 'u)))
      (setf (ffi:deref-array u '(:array :uint64-t 2) 0)
            (ldb (byte 64 0) integer)
            (ffi:deref-array u '(:array :uint64-t 2) 1)
            (ldb (byte 64 64) integer)
            (ffi:get-slot-value v 'long-float/uint128 'u)
            u))
    (ffi:get-slot-value v 'long-float/uint128 'f))
  #-(and ecl quaviver.bits/long-float)
  (error "Unable to decode long-float"))
