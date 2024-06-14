(in-package #:quaviver/schubfach)

(defclass client (quaviver/ieee754:client
                  quaviver/integer-significand:client)
  ())

(defmacro %float-decimal (client value bits significand-bits expt10 round-to-odd)
  (let ((exponent-bias (+ (ash 1 (- bits significand-bits 1)) significand-bits -2))
        (hidden-bit (ash 1 (1- significand-bits))))
    `(block %float-decimal
       (if (zerop ,value)
           (values 0 0 (floor (float-sign ,value)))
           (let* ((value-bits (the (unsigned-byte ,bits)
                                   (quaviver:float-bits ,client ,value)))
                  (significand (ldb (byte ,(1- significand-bits) 0)
                                    value-bits))
                  (exponent (ldb (byte ,(- bits significand-bits) ,(1- significand-bits))
                                 value-bits))
                  (sign (if (logbitp ,(1- bits) value-bits) -1 1))
                  (lower 0)
                  (upper 0)
                  (lower-boundary-is-closer (and (zerop significand)
                                                 (> exponent 1)
                                                 t))
                  is-even (k 0) (h 0) (expt10 0) (s 0))
             (declare (type (unsigned-byte ,bits)
                            value-bits significand lower upper s)
                      (type (unsigned-byte ,(ash bits 1))
                            expt10)
                      (boolean lower-boundary-is-closer is-even))
             (cond ((zerop exponent) ; subnormal
                    (let ((shift (- ,significand-bits (integer-length significand))))
                      (setf exponent (- 1 ,exponent-bias shift)
                            significand (ash significand shift))))
                   (t
                    (decf exponent ,exponent-bias)
                    (setf significand (logior significand ,hidden-bit))))
             (setf is-even (evenp significand)
                   significand (ash significand 2)
                   lower (if lower-boundary-is-closer
                             (1- significand)
                             (- significand 2))
                   upper (+ significand 2)
                   k (quaviver/math:floor-log10-expt2 exponent lower-boundary-is-closer)
                   h (+ exponent 1 (quaviver/math:floor-log2-expt10 (- k)))
                   expt10 (,expt10 k)
                   lower (,round-to-odd expt10 (ash lower h))
                   significand (,round-to-odd expt10 (ash significand h))
                   upper (,round-to-odd expt10 (ash upper h))
                   s (ash significand -2))
             (unless is-even
               (incf lower)
               (decf upper))
             (when (>= s 10)
               (let* ((sp (floor s 10))
                      (up-inside (<= lower (* 40 sp)))
                      (wp-inside (<= (* 40 (1+ sp)) upper)))
                 (unless (eq (not up-inside) (not wp-inside))
                   (return-from %float-decimal
                     (values (if wp-inside (1+ sp) sp)
                             (1+ k)
                             sign)))))
             (let ((u-inside (<= lower (ash s 2)))
                   (w-inside (<= (ash (1+ s) 2) upper)))
               (unless (eq (not u-inside) (not w-inside))
                 (return-from %float-decimal
                   (values (if w-inside (1+ s) s)
                           k
                           sign))))
             (let* ((mid (+ (ash s 2) 2))
                    (round-up (or (> significand mid)
                                  (and (= significand mid)
                                       (logbitp s 0)))))
               (values (if round-up (1+ s) s)
                       k
                       sign)))))))

(defmethod quaviver:float-decimal ((client client) (value single-float))
  (%float-decimal client value
                  32 24
                  quaviver/math:integer-expt10/32
                  quaviver/math:round-to-odd/32))

(defmethod quaviver:float-decimal ((client client) (value double-float))
  (%float-decimal client value
                  64 53
                  quaviver/math:integer-expt10/64
                  quaviver/math:round-to-odd/64))
