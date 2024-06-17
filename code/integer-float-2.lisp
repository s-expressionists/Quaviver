(in-package #:quaviver)

(defmacro %integer-encode-float
  ((bits-var significand exponent sign
    &key significand-size
         exponent-size
         ((:hidden-bit hidden-bit-p) nil)
         exponent-bias)
   &body body)
  (declare (ignore hidden-bit-p))
  (multiple-value-bind (forms declarations)
      (alexandria:parse-body body)
    `(let ((,bits-var (if (minusp sign)
                          ,(ash 1 (+ significand-size exponent-size))
                          0)))
       ,@declarations
       (unless (or (>= ,exponent ,(- exponent-bias))
                   (plusp (+ ,significand-size
                             ,exponent
                             ,exponent-bias)))
         (error "Unable to encode subnormal float with significand of ~a and exponent
of ~a when the significand size is ~a and the exponent size is ~a."
                ,significand ,exponent
                ,significand-size ,exponent-size))
       (incf ,exponent ,exponent-bias)
       (cond ((minusp ,exponent) ; Unadjusted subnormal
              (setf (ldb (byte (+ ,significand-size ,exponent) 0)
                         ,bits-var)
                    (ldb (byte (+ ,significand-size ,exponent) (- 1 ,exponent))
                         ,significand)))
             (t
              (setf (ldb (byte ,significand-size 0) ,bits-var) ,significand
                    (ldb (byte ,exponent-size ,significand-size) ,bits-var) ,exponent)))
       ,@forms)))

(declaim (inline ub32-sb32))
(defun ub32-sb32 (ub32)
  (if (not (zerop (ldb (byte 1 31) ub32)))
      (- ub32 #.(ash 1 32))
      ub32))

#+(or abcl allegro ccl clasp cmucl ecl lispworks mezzano sbcl)
(defmethod integer-float
    (client (result-type (eql 'single-float)) (base (eql 2)) significand exponent sign)
  (declare (ignore client))
  (%integer-encode-float
      (bits significand exponent sign
       :significand-size 23
       :exponent-size 8
       :hidden-bit t
       :exponent-bias 150)
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
      (bits significand exponent sign
       :significand-size 52
       :exponent-size 11
       :hidden-bit t
       :exponent-bias 1075)
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
