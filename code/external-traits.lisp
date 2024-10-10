(in-package #:quaviver)


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
       
(%external-traits :bfloat16 8 8)

(%external-traits :binary16 5 11)

(%external-traits :binary32 8 24)

(%external-traits :binary64 11 53)

(%external-traits :binary80 15 64)

(%external-traits :binary128 15 113)

(%external-traits :binary256 19 237)

