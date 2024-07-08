(in-package #:quaviver)

(defmacro %traits (type most-positive least-positive least-positive-normalized)
  (flet ((float-exponent (float)
           (multiple-value-bind (significand exponent)
               (integer-decode-float float)
             (+ exponent (integer-length significand) -2))))
    (let* ((full-significand-size (float-digits (coerce 0 type)))
           (sign-size 1)
           (most-positive (symbol-value most-positive))
           (least-positive (symbol-value least-positive))
           (least-positive-normalized (symbol-value least-positive-normalized))
           (most-exponent (float-exponent most-positive))
           (least-normalized-exponent (float-exponent least-positive-normalized))
           (subnormalp (/= least-positive least-positive-normalized))
           (exponent-size (integer-length (- most-exponent
                                             least-normalized-exponent
                                             -1)))
           (hidden-bit-p (ldb-test (byte 3 0)
                                   (+ 1
                                      full-significand-size
                                      exponent-size)))
           (storage-size (if hidden-bit-p
                             (+ full-significand-size exponent-size)
                             (+ 1 full-significand-size exponent-size)))
           (significand-size (if hidden-bit-p
                                 (1- full-significand-size)
                                 full-significand-size))
           (non-number-p (/= (logcount (- most-exponent least-normalized-exponent))
                             exponent-size))
           (exponent-bias (- full-significand-size least-normalized-exponent 1))
           (max-exponent (- most-exponent full-significand-size -2))
           (min-exponent (if subnormalp
                             (- 2 exponent-bias
                                full-significand-size)
                             (- 1 exponent-bias))))
      `(progn
         (defmethod storage-size ((type (eql ',type)))
           ,storage-size)

         (defmethod significand-bytespec ((type (eql ',type)))
           (byte ,significand-size 0))

         (defmethod significand-byte-form ((type (eql ',type)))
           '(byte ,significand-size 0))

         (defmethod significand-size ((type (eql ',type)))
           ,full-significand-size)

         (defmethod exponent-bytespec ((type (eql ',type)))
           (byte ,exponent-size ,significand-size))

         (defmethod exponent-byte-form ((type (eql ',type)))
           '(byte ,exponent-size ,significand-size))

         (defmethod exponent-size ((type (eql ',type)))
           ,exponent-size)

         (defmethod sign-bytespec ((type (eql ',type)))
           (byte ,sign-size ,(+ exponent-size significand-size)))

         (defmethod sign-byte-form ((type (eql ',type)))
           '(byte ,sign-size ,(+ exponent-size significand-size)))

         (defmethod sign-size ((type (eql ',type)))
           ,sign-size)

         (defmethod nan-payload-bytespec ((type (eql ',type)))
           (byte ,(1- significand-size) 0))

         (defmethod nan-payload-byte-form ((type (eql ',type)))
           '(byte ,(1- significand-size) 0))

         (defmethod nan-type-bytespec ((type (eql ',type)))
           (byte 1 ,(1- significand-size)))

         (defmethod nan-type-byte-form ((type (eql ',type)))
           '(byte 1 ,(1- significand-size)))

         (defmethod hidden-bit-p ((type (eql ',type)))
           ,hidden-bit-p)

         (defmethod subnormalp ((type (eql ',type)))
           ,subnormalp)

         (defmethod non-number-p ((type (eql ',type)))
           ,non-number-p)

         (defmethod exponent-bias ((type (eql ',type)))
           ,exponent-bias)

         (defmethod max-exponent ((type (eql ',type)))
           ,max-exponent)

         (defmethod min-exponent ((type (eql ',type)))
           ,min-exponent)

         (defmethod arithmetic-size ((type (eql ',type)))
           ,(ash 1 (integer-length (+ 6 full-significand-size))))))))

#+quaviver/short-float
(%traits short-float
         most-positive-short-float
         least-positive-short-float
         least-positive-normalized-short-float)

(%traits single-float
         most-positive-single-float
         least-positive-single-float
         least-positive-normalized-single-float)

(%traits double-float
         most-positive-double-float
         least-positive-double-float
         least-positive-normalized-double-float)

#+quaviver/long-float
(%traits long-float
         most-positive-long-float
         least-positive-long-float
         least-positive-normalized-long-float)
