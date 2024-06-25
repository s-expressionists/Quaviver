(in-package #:quaviver)

(defmacro %integer-encode-float
    ((type bits-var significand exponent sign) &body body)
  (with-accessors ((storage-size storage-size)
                   (significand-bytespec significand-bytespec)
                   (exponent-bytespec exponent-bytespec)
                   (sign-bytespec sign-bytespec)
                   (nan-payload-bytespec nan-payload-bytespec)
                   (nan-type-bytespec nan-type-bytespec)
                   (hidden-bit-p hidden-bit-p)
                   (exponent-bias exponent-bias))
      type
    (multiple-value-bind (forms declarations)
        (alexandria:parse-body body)
      `(let ((,bits-var 0))
         ,@declarations
         (declare (type (unsigned-byte ,storage-size) ,bits-var))
         (when (minusp ,sign)
           (setf (ldb ',sign-bytespec ,bits-var) 1))
         (cond ((keywordp exponent)
                (setf (ldb ',exponent-bytespec ,bits-var)
                      ,(1- (ash 1 (byte-size exponent-bytespec))))
                (ecase exponent
                  (:infinity)
                  (:quiet-nan
                   (setf (ldb ',nan-type-bytespec ,bits-var) 1
                         (ldb ',nan-payload-bytespec ,bits-var) ,significand))
                  (:signaling-nan
                   (setf (ldb ',nan-payload-bytespec ,bits-var)
                         (if (zerop ,significand) 1 ,significand)))))
               (t
                (unless (zerop ,significand)
                  (let ((shift (- ,(if hidden-bit-p
                                       (1+ (byte-size significand-bytespec))
                                       (byte-size significand-bytespec))
                                  (integer-length ,significand))))
                    (setf ,significand (ash ,significand shift))
                    (decf ,exponent shift)))
                (cond ((zerop ,significand)
                       (setf (ldb ',exponent-bytespec ,bits-var) ,exponent-bias))
                      (t
                       (unless (and (< ,exponent
                                       ,(- (1- (ash 1 (byte-size exponent-bytespec)))
                                           exponent-bias))
                                    (or (>= ,exponent ,(- exponent-bias))
                                        (plusp (+ ,(byte-size significand-bytespec)
                                                  ,exponent
                                                  ,exponent-bias))))
                         (error "Unable to encode float with significand of ~a and ~
                                 exponent of ~a when~%the significand size is ~a and ~
                                 the exponent size is ~a."
                                ,significand ,exponent
                                ,(byte-size significand-bytespec)
                                ,(byte-size exponent-bytespec)))
                       (incf ,exponent ,exponent-bias)
                       (cond ((plusp ,exponent)
                              (setf (ldb ',significand-bytespec ,bits-var)
                                    ,significand
                                    (ldb ',exponent-bytespec ,bits-var)
                                    ,exponent))
                             (t ; Unadjusted subnormal
                              (setf (ldb (byte (+ ,(byte-size significand-bytespec)
                                                  ,exponent)
                                               ,(byte-position significand-bytespec))
                                         ,bits-var)
                                    (ldb (byte (+ ,(byte-size significand-bytespec)
                                                  ,exponent)
                                               (- ,(1+ (byte-position significand-bytespec))
                                                  ,exponent))
                                         ,significand))))))))
         ,@forms))))

(declaim (inline ub32-sb32))
(defun ub32-sb32 (ub32)
  (if (logbitp 31 ub32)
      (- ub32 #.(ash 1 32))
      ub32))

#+(or abcl allegro ccl clasp cmucl ecl lispworks mezzano sbcl)
(defmethod integer-float
    (client (result-type (eql 'single-float)) (base (eql 2)) significand exponent sign)
  (declare (ignore client))
  (%integer-encode-float
      (single-float bits significand exponent sign)
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
  (declare (ignore client))
  (%integer-encode-float
      (double-float bits significand exponent sign)
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
