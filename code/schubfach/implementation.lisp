(in-package #:quaviver/schubfach)

(deftype rounding ()
  `(member :toward-zero :away-from-zero))

(defclass client (quaviver/trailing-zeros:client)
  ((rounding :accessor rounding
             :initarg :rounding
             :initform :away-from-zero)))

(defmethod initialize-instance :after ((client client) &rest initargs &key)
  (declare (ignore initargs))
  (check-type (rounding client) rounding))

(defmacro %schubfach (client value type expt10 round-to-odd)
  (with-accessors ((arithmetic-size quaviver:arithmetic-size)
                   (significand-size quaviver:significand-size))
      type
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
                      (k (quaviver/math:floor-log-expt 10 2 exponent lower-boundary-is-closer))
                      (h (+ exponent 1 (quaviver/math:floor-log-expt 2 10 (- k))))
                      (expt10 (,expt10 k)))
                 (declare (type (unsigned-byte ,(ash arithmetic-size 1))
                                expt10)
                          (type boolean
                                lower-boundary-is-closer is-even)
                          (type (integer 0 4)
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
                                               (or (eq (rounding ,client) :away-from-zero)
                                                   (logbitp s 0))))))
                       (declare (type (unsigned-byte ,word-size)
                                      mid))
                       (values (if round-up (1+ s) s)
                               k
                               sign)))))))))))

#+clisp
(defmethod quaviver:float-integer ((client client) (base (eql 10)) (value short-float))
  (%schubfach client value
              short-float
              quaviver/math:expt10/32
              quaviver/math:round-to-odd/32))

(defmethod quaviver:float-integer ((client client) (base (eql 10)) (value single-float))
  (%schubfach client value
              single-float
              quaviver/math:expt10/32
              quaviver/math:round-to-odd/32))

(defmethod quaviver:float-integer ((client client) (base (eql 10)) (value double-float))
  (%schubfach client value
              double-float
              quaviver/math:expt10/64
              quaviver/math:round-to-odd/64))

#+quaviver/long-float
(defmethod quaviver:float-integer ((client client) (base (eql 10)) (value long-float))
  (%schubfach client value
              long-float
              quaviver/math:expt10/128
              quaviver/math:round-to-odd/128))
