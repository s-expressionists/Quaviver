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
           (byte ,(- significand-size
                     (if hidden-bit-p 1 2))
                 0))

         (defmethod nan-payload-byte-form ((type (eql ',type)))
           '(byte ,(- significand-size
                      (if hidden-bit-p 1 2))
                  0))

         (defmethod nan-type-bytespec ((type (eql ',type)))
           (byte 1 ,(- significand-size
                       (if hidden-bit-p 1 2))))

         (defmethod nan-type-byte-form ((type (eql ',type)))
           '(byte 1 ,(- significand-size
                      (if hidden-bit-p 1 2))))

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

(defun find-implementation-type (type exponent-size significand-size)
  (if (and (member type '(short-float single-float double-float long-float))
           t)
      type
      (let* ((types (remove-if (lambda (type)
                                 (not (eq type (implementation-type type))))
                               '(short-float single-float double-float long-float)))
             (exact-type (find-if (lambda (type)
                                    (and (= (exponent-size type)
                                            exponent-size)
                                         (= (significand-size type)
                                            significand-size)))
                                  types)))
        (if exact-type
            (values exact-type t)
            (values (or (find-if (lambda (type)
                                   (and (>= (exponent-size type)
                                            exponent-size)
                                        (>= (significand-size type)
                                            significand-size)))
                                 types)
                        (car (last types)))
                    nil)))))

(defun traits-from-sizes (type exponent-size significand-size)
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
         (core-type-p (and (member type '(short-float single-float double-float long-float))
                           t)))
    (multiple-value-bind (implementation-type exactp)
        (find-implementation-type type exponent-size significand-size)
      `(:storage-size ,storage-size
        :significand-byte-form (byte ,stored-significand-size 0)
        :significand-size ,significand-size
        :exponent-byte-form (byte ,exponent-size ,stored-significand-size)
        :exponent-size ,exponent-size
        :sign-byte-form (byte ,sign-size ,(+ exponent-size stored-significand-size))
        :sign-size ,sign-size
        :nan-payload-byte-form (byte ,(- stored-significand-size
                                         (if hidden-bit-p 1 2))
                                     0)
        :nan-type-byte-form (byte 1 ,(- stored-significand-size
                                        (if hidden-bit-p 1 2)))
        :hidden-bit-p ,hidden-bit-p
        :subnormalp ,subnormalp
        :non-number-p ,non-number-p
        :internal-base ,internal-base
        :exponent-bias ,exponent-bias
        :max-exponent ,max-exponent
        :min-exponent ,min-exponent
        :arithmetic-size ,(ash 1 (integer-length (+ 6 significand-size)))
        :exact-implementation-type-p ,exactp
        :external-type ,type
        :implementation-type ,implementation-type))))

(defmacro %boot-traits (type)
  (destructuring-bind (&key storage-size significand-byte-form significand-size
                            exponent-byte-form exponent-size sign-byte-form sign-size
                            nan-payload-byte-form nan-type-byte-form hidden-bit-p subnormalp
                            non-number-p internal-base exponent-bias max-exponent min-exponent
                            arithmetic-size exact-implementation-type-p external-type
                            implementation-type)
      (traits-from-sizes type (get type :exponent-size) (get type :significand-size))
    `(progn
       (defmethod storage-size ((type (eql ',type)))
         ,storage-size)

       (defmethod significand-bytespec ((type (eql ',type)))
         ,significand-byte-form)

       (defmethod significand-byte-form ((type (eql ',type)))
         ',significand-byte-form)

       (defmethod significand-size ((type (eql ',type)))
         ,significand-size)

       (defmethod exponent-bytespec ((type (eql ',type)))
         ,exponent-byte-form)

       (defmethod exponent-byte-form ((type (eql ',type)))
         ',exponent-byte-form)

       (defmethod exponent-size ((type (eql ',type)))
         ,exponent-size)

       (defmethod sign-bytespec ((type (eql ',type)))
         ,sign-byte-form)

       (defmethod sign-byte-form ((type (eql ',type)))
         ',sign-byte-form)

       (defmethod sign-size ((type (eql ',type)))
         ,sign-size)

       (defmethod nan-payload-bytespec ((type (eql ',type)))
         ,nan-payload-byte-form)

       (defmethod nan-payload-byte-form ((type (eql ',type)))
         ',nan-payload-byte-form)

       (defmethod nan-type-bytespec ((type (eql ',type)))
         ,nan-type-byte-form)

       (defmethod nan-type-byte-form ((type (eql ',type)))
         ',nan-type-byte-form)

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
         ,arithmetic-size))))

#-quaviver/boot
(%traits short-float
         most-positive-short-float
         least-positive-short-float
         least-positive-normalized-short-float)

#+quaviver/boot
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

#-quaviver/boot
(%traits long-float
         most-positive-long-float
         least-positive-long-float
         least-positive-normalized-long-float)

#+quaviver/boot
(%boot-traits long-float)
