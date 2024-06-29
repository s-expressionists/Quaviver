(in-package #:quaviver)

(defmacro %integer-encode-float
    ((bits-var client type significand exponent sign) &body body)
  (with-accessors ((storage-size storage-size)
                   (significand-bytespec significand-bytespec)
                   (significand-byte-form significand-byte-form)
                   (exponent-bytespec exponent-bytespec)
                   (exponent-byte-form exponent-byte-form)
                   (sign-byte-form sign-byte-form)
                   (nan-payload-byte-form nan-payload-byte-form)
                   (nan-type-byte-form nan-type-byte-form)
                   (hidden-bit-p hidden-bit-p)
                   (exponent-bias exponent-bias)
                   (min-exponent min-exponent)
                   (max-exponent max-exponent)
                   (significand-size significand-size))
      type
    (multiple-value-bind (forms declarations)
        (alexandria:parse-body body)
      (let ((exponent-var (gensym))
            (significand-var (gensym)))
        `(let ((,bits-var 0)
               (,exponent-var ,exponent)
               (,significand-var ,significand))
           ,@declarations
           (declare (type (unsigned-byte ,storage-size) ,bits-var))
           (when (minusp ,sign)
             (setf (ldb ,sign-byte-form ,bits-var) 1))
           (cond ((keywordp exponent)
                  (setf (ldb ,exponent-byte-form ,bits-var)
                        ,(1- (ash 1 (byte-size exponent-bytespec))))
                  (ecase exponent
                    (:infinity)
                    (:quiet-nan
                     (setf (ldb ,nan-type-byte-form ,bits-var) 1
                           (ldb ,nan-payload-byte-form ,bits-var) ,significand-var))
                    (:signaling-nan
                     (setf (ldb ,nan-payload-byte-form ,bits-var)
                           (if (zerop ,significand-var) 1 ,significand-var)))))
                 ((zerop ,significand-var))
                 (t
                  (let ((shift (- ,significand-size
                                  (integer-length ,significand-var))))
                    (setf ,significand-var (ash ,significand-var shift))
                    (decf ,exponent-var shift))
                  (cond ((< ,exponent-var ,min-exponent)
                         (error 'floating-point-underflow
                                :operation 'integer-float
                                :operands (list ,client ',type 2
                                                ,significand ,exponent ,sign)))
                        ((> ,exponent-var ,max-exponent)
                         (error 'floating-point-overflow
                                :operation 'integer-float
                                :operands (list ,client ',type 2
                                                ,significand ,exponent ,sign)))
                        (t
                         (incf ,exponent-var ,exponent-bias)
                         (cond ((plusp ,exponent-var)
                                (setf (ldb ,significand-byte-form ,bits-var)
                                      ,significand-var
                                      (ldb ,exponent-byte-form ,bits-var)
                                      ,exponent-var))
                               (t ; Unadjusted subnormal
                                (setf (ldb (byte (+ ,(byte-size significand-bytespec)
                                                    ,exponent-var)
                                                 ,(byte-position significand-bytespec))
                                           ,bits-var)
                                      (ldb (byte (+ ,(byte-size significand-bytespec)
                                                    ,exponent-var)
                                                 (- ,(1+ (byte-position significand-bytespec))
                                                    ,exponent-var))
                                           ,significand-var))))))))
           ,@forms)))))

(declaim (inline ub32-sb32))
(defun ub32-sb32 (ub32)
  (if (logbitp 31 ub32)
      (- ub32 #.(ash 1 32))
      ub32))

#+(or abcl allegro ccl clasp cmucl ecl lispworks mezzano sbcl)
(defmethod integer-float
    (client (result-type (eql 'single-float)) (base (eql 2)) significand exponent sign)
  (%integer-encode-float
      (bits client single-float significand exponent sign)
    #+abcl
    (system:make-single-float bits)
    #+allegro
    (excl:shorts-to-single-float (ldb (byte 16 16) bits)
                                 (ldb (byte 16 0) bits))
    #+ccl
    (ccl::host-single-float-from-unsigned-byte-32 bits)
    #+clasp
    (ext:bits-to-single-float bits)
    #+cmucl
    (kernel:make-single-float (ub32-sb32 bits))
    #+ecl
    (system:bits-single-float bits)
    #+lispworks
    (let ((v (sys:make-typed-aref-vector 4)))
      (declare (optimize (speed 3) (float 0) (safety 0))
               (dynamic-extent v))
      (setf (sys:typed-aref '(unsigned-byte 32) v 0) bits)
      (sys:typed-aref 'single-float v 0))
    #+mezzano
    (mezzano.extensions:ieee-binary32-to-single-float bits)
    #+sbcl
    (sb-kernel:make-single-float (ub32-sb32 bits))))

#+(or abcl allegro ccl clasp cmucl ecl lispworks mezzano sbcl)
(defmethod integer-float
    (client (result-type (eql 'double-float)) (base (eql 2)) significand exponent sign)
  (%integer-encode-float
      (bits client double-float significand exponent sign)
    #+abcl
    (system:make-double-float bits)
    #+allegro
    (excl:shorts-to-double-float (ldb (byte 16 48) bits)
                                 (ldb (byte 16 32) bits)
                                 (ldb (byte 16 16) bits)
                                 (ldb (byte 16 0) bits))
    #+ccl
    (ccl::double-float-from-bits (ldb (byte 32 32) bits)
                                 (ldb (byte 32 0) bits))
    #+clasp
    (ext:bits-to-double-float bits)
    #+cmucl
    (kernel:make-double-float (ub32-sb32 (ldb (byte 32 32) bits))
                              (ldb (byte 32 0) bits))
    #+ecl
    (system:bits-double-float bits)
    #+lispworks
    (let ((upper (ldb (byte 32 32) bits))
          (lower (ldb (byte 32 0) bits))
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
    (mezzano.extensions:ieee-binary64-to-double-float bits)
    #+sbcl
    (sb-kernel:make-double-float (ub32-sb32 (ldb (byte 32 32) bits))
                                 (ldb (byte 32 0) bits))))

#+quaviver/long-float
(defmethod integer-float
    (client (result-type (eql 'long-float)) (base (eql 2)) significand exponent sign)
  (%integer-encode-float
      (bits client long-float significand exponent sign)
    #+quaviver.bits/long-float
    (quaviver.bits:bits-long-float bits)
    #+(and ecl (not quaviver.bits/long-float))
    (system:bits-long-float bits)))
