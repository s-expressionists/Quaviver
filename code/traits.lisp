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

         (defmethod internal-base ((type (eql ',type)))
           ,(float-radix (coerce 0 type)))

         (defmethod exponent-bias ((type (eql ',type)))
           ,exponent-bias)

         (defmethod max-exponent ((type (eql ',type)))
           ,max-exponent)

         (defmethod min-exponent ((type (eql ',type)))
           ,min-exponent)

         (defmethod arithmetic-size ((type (eql ',type)))
           ,(ash 1 (integer-length (+ 6 full-significand-size))))))))

(defmacro %boot-traits (type)
  (let* ((exponent-size (get type :exponent))
         (significand-size (get type :significand))
         (hidden-bit-p (= 1 (logcount (+ exponent-size significand-size))))
         (storage-size (if hidden-bit-p
                           (+ exponent-size significand-size)
                           (+ 1 exponent-size significand-size)))
         (stored-significand-size (if hidden-bit-p
                                      (1- significand-size)
                                      significand-size))
      	 (sign-size 1)
         (subnormalp t)
         (non-number-p t)
         (internal-base 2)
         (exponent-bias (+ (ash 1 (1- exponent-size)) significand-size -2))
         (min-exponent (- 2 exponent-bias significand-size))
         (max-exponent (- (ash 1 (1- exponent-size)) significand-size)))
    `(progn
       (defmethod storage-size ((type (eql ',type)))
         ,storage-size)

       (defmethod significand-bytespec ((type (eql ',type)))
         (byte ,stored-significand-size 0))

       (defmethod significand-byte-form ((type (eql ',type)))
         '(byte ,stored-significand-size 0))

       (defmethod significand-size ((type (eql ',type)))
         ,significand-size)

       (defmethod exponent-bytespec ((type (eql ',type)))
         (byte ,exponent-size ,stored-significand-size))

       (defmethod exponent-byte-form ((type (eql ',type)))
         '(byte ,exponent-size ,stored-significand-size))

       (defmethod exponent-size ((type (eql ',type)))
         ,exponent-size)

       (defmethod sign-bytespec ((type (eql ',type)))
         (byte ,sign-size ,(+ exponent-size stored-significand-size)))

       (defmethod sign-byte-form ((type (eql ',type)))
         '(byte ,sign-size ,(+ exponent-size stored-significand-size)))

       (defmethod sign-size ((type (eql ',type)))
         ,sign-size)

       (defmethod nan-payload-bytespec ((type (eql ',type)))
         (byte ,(1- stored-significand-size) 0))

       (defmethod nan-payload-byte-form ((type (eql ',type)))
         '(byte ,(1- stored-significand-size) 0))

       (defmethod nan-type-bytespec ((type (eql ',type)))
         (byte 1 ,(1- stored-significand-size)))

       (defmethod nan-type-byte-form ((type (eql ',type)))
         '(byte 1 ,(1- stored-significand-size)))

       (defmethod hidden-bit-p ((type (eql ',type)))
         ,hidden-bit-p)

       (defmethod subnormalp ((type (eql ',type)))
         ,subnormalp)

       (defmethod non-number-p ((type (eql ',type)))
         ,non-number-p)

       (defmethod internal-base ((type (eql ',type)))
         ,internal-base)

       (defmethod exponent-bias ((type (eql ',type)))
         ,exponent-bias)

       (defmethod max-exponent ((type (eql ',type)))
         ,max-exponent)

       (defmethod min-exponent ((type (eql ',type)))
         ,min-exponent)

       (defmethod arithmetic-size ((type (eql ',type)))
         ,(ash 1 (integer-length (+ 6 significand-size)))))))


(defmacro %external-traits (type exponent-size significand-size)
  (let* ((hidden-bit-p (= 1 (logcount (+ exponent-size significand-size))))
         (storage-size (if hidden-bit-p
                           (+ exponent-size significand-size)
                           (+ 1 exponent-size significand-size)))
         (stored-significand-size (if hidden-bit-p
                                      (1- significand-size)
                                      significand-size))
      	 (sign-size 1)
         (subnormalp t)
         (non-number-p t)
         (internal-base 2)
         (exponent-bias (+ (ash 1 (1- exponent-size)) significand-size -2))
         (min-exponent (- 2 exponent-bias significand-size))
         (max-exponent (- (ash 1 (1- exponent-size)) significand-size))
         (implementation-type (loop for (type . tail) on '(#+quaviver/short-float short-float
                                                           single-float
                                                           double-float
                                                           #+quaviver/long-float long-float)
                                    when (or (null tail)
                                             (and (>= (exponent-size type)
                                                      exponent-size)
                                                  (>= (significand-size type)
                                                      significand-size)))
                                      return type))
         (exactp (loop for (type . tail) on '(#+quaviver/short-float short-float
                                              single-float
                                              double-float
                                              #+quaviver/long-float long-float)
                       when (or (null tail)
                                (and (>= (exponent-size type)
                                         exponent-size)
                                     (>= (significand-size type)
                                         significand-size)))
                         return (and (= (exponent-size type)
                                        exponent-size)
                                     (= (significand-size type)
                                        significand-size)))))
    `(progn
       (defmethod storage-size ((type (eql ',type)))
         ,storage-size)

       (defmethod significand-bytespec ((type (eql ',type)))
         (byte ,stored-significand-size 0))

       (defmethod significand-byte-form ((type (eql ',type)))
         '(byte ,stored-significand-size 0))

       (defmethod significand-size ((type (eql ',type)))
         ,significand-size)

       (defmethod exponent-bytespec ((type (eql ',type)))
         (byte ,exponent-size ,stored-significand-size))

       (defmethod exponent-byte-form ((type (eql ',type)))
         '(byte ,exponent-size ,stored-significand-size))

       (defmethod exponent-size ((type (eql ',type)))
         ,exponent-size)

       (defmethod sign-bytespec ((type (eql ',type)))
         (byte ,sign-size ,(+ exponent-size stored-significand-size)))

       (defmethod sign-byte-form ((type (eql ',type)))
         '(byte ,sign-size ,(+ exponent-size stored-significand-size)))

       (defmethod sign-size ((type (eql ',type)))
         ,sign-size)

       (defmethod nan-payload-bytespec ((type (eql ',type)))
         (byte ,(1- stored-significand-size) 0))

       (defmethod nan-payload-byte-form ((type (eql ',type)))
         '(byte ,(1- stored-significand-size) 0))

       (defmethod nan-type-bytespec ((type (eql ',type)))
         (byte 1 ,(1- stored-significand-size)))

       (defmethod nan-type-byte-form ((type (eql ',type)))
         '(byte 1 ,(1- stored-significand-size)))

       (defmethod hidden-bit-p ((type (eql ',type)))
         ,hidden-bit-p)

       (defmethod subnormalp ((type (eql ',type)))
         ,subnormalp)

       (defmethod non-number-p ((type (eql ',type)))
         ,non-number-p)

       (defmethod internal-base ((type (eql ',type)))
         ,internal-base)

       (defmethod exponent-bias ((type (eql ',type)))
         ,exponent-bias)

       (defmethod max-exponent ((type (eql ',type)))
         ,max-exponent)

       (defmethod min-exponent ((type (eql ',type)))
         ,min-exponent)

       (defmethod arithmetic-size ((type (eql ',type)))
         ,(ash 1 (integer-length (+ 6 significand-size))))

       (defmethod implementation-type ((type (eql ',type)))
         ',implementation-type)

       (defmethod exact-implementation-type-p ((type (eql ',type)))
         ,exactp)

       (defmethod external-type ((type (eql ',type)))
         ',type)

       ,@(when exactp
           `((defmethod external-type ((type (eql ',implementation-type)))
               ',type))))))

#+quaviver/short-float (not quaviver/boot))
(%traits short-float
         most-positive-short-float
         least-positive-short-float
         least-positive-normalized-short-float)

#+(and quaviver/short-float quaviver/boot)
(%boot-traits short-float)

#-quaviver/boot
(%traits single-float
         most-positive-single-float
         least-positive-single-float
         least-positive-normalized-single-float)

#+quaviver/boot
(%boot-traits single-float)

#-quaviver/boot
(%traits double-float
         most-positive-double-float
         least-positive-double-float
         least-positive-normalized-double-float)

#+quaviver/boot
(%boot-traits double-float)

#+(and quaviver/long-float (not quaviver/boot))
(%traits long-float
         most-positive-long-float
         least-positive-long-float
         least-positive-normalized-long-float)

#+(and quaviver/long-float quaviver/boot)
(%boot-traits long-float)

(%external-traits :bfloat16 8 8)

(%external-traits :binary16 5 11)

(%external-traits :binary32 8 24)

(%external-traits :binary64 11 53)

(%external-traits :binary80 15 64)

(%external-traits :binary128 15 113)

(%external-traits :binary256 19 237)
