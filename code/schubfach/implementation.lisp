(in-package #:quaviver/schubfach)

(defclass client (quaviver/trailing-zeros:client)
  ())

(defmacro %schubfach (client value bits significand-size expt10 round-to-odd)
  (let ((word-size (+ significand-size 6)))
    `(block %schubfach
       (multiple-value-bind (significand exponent sign)
           (quaviver:float-integer ,client 2 ,value)
         (declare (type (unsigned-byte ,word-size) significand)
                  (type (or fixnum keyword) exponent)
                  (type (integer -1 1) sign))
         (if (or (not (numberp exponent))
                 (zerop significand))
             (values significand exponent sign)
             (let* ((lower-boundary-is-closer (= significand ,(ash 1 (1- significand-size))))
                    (is-even (evenp significand))
                    (k (quaviver/math:floor-log10-expt2 exponent lower-boundary-is-closer))
                    (h (+ exponent 1 (quaviver/math:floor-log2-expt10 (- k))))
                    (expt10 (,expt10 k)))
               (declare (type (unsigned-byte ,(ash bits 1))
                              expt10)
                        (type boolean
                              lower-boundary-is-closer is-even)
                        (type (integer 1 4)
                              h))
               (setf significand (ash significand 2))
               (let ((lower (,round-to-odd expt10
                                           (ash (if lower-boundary-is-closer
                                                    (1- significand)
                                                    (- significand 2))
                                                h)))
                     (upper (,round-to-odd expt10
                                           (ash (+ significand 2)
                                                h))))
                 (declare (type (unsigned-byte ,word-size)
                                lower upper))
                 (setf significand (,round-to-odd expt10 (ash significand h)))
                 (let ((s (ash significand -2)))
                   (declare (type (unsigned-byte ,word-size)
                                  s))
                   (unless is-even
                     (incf lower)
                     (decf upper))
                   (when (>= s 10)
                     (let* ((sp (floor s 10))
                            (up-inside (<= lower (* 40 sp)))
                            (wp-inside (<= (* 40 (1+ sp)) upper)))
                       (declare (type (unsigned-byte ,word-size)
                                      sp))
                       (unless (eq (not up-inside) (not wp-inside))
                         (return-from %schubfach
                           (values (if wp-inside (1+ sp) sp)
                                   (1+ k)
                                   sign)))))
                   (let ((u-inside (<= lower (ash s 2)))
                         (w-inside (<= (ash (1+ s) 2) upper)))
                     (unless (eq (not u-inside) (not w-inside))
                       (return-from %schubfach
                         (values (if w-inside (1+ s) s)
                                 k
                                 sign))))
                   (let* ((mid (+ (ash s 2) 2))
                          (round-up (or (> significand mid)
                                        (and (= significand mid)
                                             (logbitp s 0)))))
                     (declare (type (unsigned-byte ,word-size)
                                    mid))
                     (values (if round-up (1+ s) s)
                             k
                             sign))))))))))

(defmethod quaviver:float-integer ((client client) (base (eql 10)) (value single-float))
  (%schubfach client value
              32 24
              quaviver/math:expt10/32
              quaviver/math:round-to-odd/32))

(defmethod quaviver:float-integer ((client client) (base (eql 10)) (value double-float))
  (%schubfach client value
              64 53
              quaviver/math:expt10/64
              quaviver/math:round-to-odd/64))

#+(and ecl long-float)
(defmethod quaviver:float-integer ((client client) (base (eql 10)) (value long-float))
  (%schubfach client value
              128 64
              quaviver/math:expt10/128
              quaviver/math:round-to-odd/128))
