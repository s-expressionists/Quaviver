(in-package #:quaviver/schubfach)

(deftype rounding ()
  `(member :to-even
           :to-odd
           :toward-zero
           :away-from-zero))

(defclass client (quaviver/trailing-zeros:client)
  ((rounding :accessor rounding
             :initarg :rounding
             :initform :away-from-zero)))

(defmethod initialize-instance :after ((client client) &rest initargs &key)
  (declare (ignore initargs))
  (check-type (rounding client) rounding))

(defmacro %schubfach (client value type round-to-odd)
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
                      (expt10 (quaviver/math:expt ,arithmetic-size 10 k)))
                 (declare (type boolean
                                lower-boundary-is-closer is-even)
                          (type (integer 0 4)
                                h)
                          (type (quaviver/math:arithmetic-word ,arithmetic-size 2)
                                expt10)
                          (dynamic-extent expt10))
                 (setf significand (ash significand 2))
                 (let ((lower (,round-to-odd (ash (if lower-boundary-is-closer
                                                      (1- significand)
                                                      (- significand 2))
                                                  h)
                                             expt10))
                       (upper (,round-to-odd (ash (+ significand 2)
                                                  h)
                                             expt10)))
                   (declare (type (unsigned-byte ,word-size)
                                  lower upper))
                   (setf significand (,round-to-odd (ash significand h) expt10))
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
                     (case (rounding ,client)
                       (:away-from-zero ; ?1?
                        (values (if (logbitp 1 significand)
                                    (1+ s)
                                    s)
                                k
                                sign))
                       (:toward-zero ; ?11
                        (values (if (= (ldb (byte 2 0) significand) #b11)
                                    (1+ s)
                                    s)
                                k
                                sign))
                       (:to-even ; ?11 or 110
                        (values (if (and (logbitp 1 significand)
                                         (or (logbitp 0 significand)
                                             (logbitp 2 significand)))
                                    (1+ s)
                                    s)
                                k
                                sign))
                       (otherwise ; ?11 or 010
                        (values (if (and (logbitp 1 significand)
                                         (or (logbitp 0 significand)
                                             (not (logbitp 2 significand))))
                                    (1+ s)
                                    s)
                                k
                                sign))))))))))))

#+clisp
(defmethod quaviver:float-integer ((client client) (base (eql 10)) (value short-float))
  (%schubfach client value
              short-float
              quaviver/math:round-to-odd/32-64))

(defmethod quaviver:float-integer ((client client) (base (eql 10)) (value single-float))
  (%schubfach client value
              single-float
              quaviver/math:round-to-odd/32-64))

(defmethod quaviver:float-integer ((client client) (base (eql 10)) (value double-float))
  (%schubfach client value
              double-float
              quaviver/math:round-to-odd/64-128))

#+quaviver/long-float
(defmethod quaviver:float-integer ((client client) (base (eql 10)) (value long-float))
  (%schubfach client value
              long-float
              quaviver/math:round-to-odd/128-256))
