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

(defmacro %schubfach (client type value)
  (with-accessors ((arithmetic-size quaviver:arithmetic-size)
                   (significand-size quaviver:significand-size)
                   (min-exponent quaviver:min-exponent)
                   (max-exponent quaviver:max-exponent))
      type
    (let ((word-size (+ significand-size 6)))
      `(block %schubfach
         (multiple-value-bind (significand exponent sign)
             ,(quaviver:float-primitive-triple-form type value)
           (declare (type (unsigned-byte ,word-size) significand)
                    (type (or keyword quaviver:exponent-word)
                          exponent)
                    (type fixnum sign))
           (when (or (keywordp exponent)
                     (zerop significand))
             (return-from %schubfach
               (values significand exponent sign)))
           (let* ((lower-boundary-is-closer (= (logcount significand) 1))
                  (is-even (evenp significand))
                  (k (quaviver.math:floor-log-expt 10 2 exponent lower-boundary-is-closer))
                  (h (+ exponent 1 (quaviver.math:floor-log-expt 2 10 (- k))))
                  (expt10 (quaviver.math:expt ,arithmetic-size 10 k)))
             (declare (type boolean
                            lower-boundary-is-closer is-even)
                      (type quaviver:exponent-word
                            k)
                      (type (integer 0 4)
                            h)
                      (type (quaviver.math:arithmetic-word ,arithmetic-size 2)
                            expt10)
                      (dynamic-extent expt10)
                      (optimize speed))
             (setf significand (ash significand 2))
             (let ((lower (quaviver.math:round-to-odd
                           ,arithmetic-size
                           (ash (if lower-boundary-is-closer
                                    (1- significand)
                                    (- significand 2))
                                h)
                           expt10))
                   (upper (quaviver.math:round-to-odd
                           ,arithmetic-size
                           (ash (+ significand 2)
                                h)
                           expt10))
                   (s 0)
                   (s+1 0)
                   round-up)
               (declare (type (unsigned-byte ,word-size)
                              lower upper)
                        (type (unsigned-byte ,(- word-size 2))
                              s s+1)
                        (type boolean round-up))
               (setf significand (quaviver.math:round-to-odd
                                  ,arithmetic-size
                                  (ash significand h) expt10))
               (unless is-even
                 (incf lower)
                 (decf upper))
               (setf s (floor significand 40)
                     s+1 (1+ s)
                     round-up (<= (* 40 s+1) upper))
               (unless (eq (not (<= lower (* 40 s)))
                           (not round-up))
                 (return-from %schubfach
                   (values (if round-up s+1 s)
                           (1+ k)
                           sign)))
               (setf s (ash significand -2)
                     s+1 (1+ s)
                     round-up (<= (ash s+1 2) upper))
               (unless (eq (not (<= lower (ash s 2)))
                           (not round-up))
                 (return-from %schubfach
                   (values (if round-up s+1 s)
                           k
                           sign)))
               (case (rounding ,client)
                 (:away-from-zero ; ?1?
                  (values (if (logbitp 1 significand)
                              s+1
                              s)
                          k
                          sign))
                 (:toward-zero ; ?11
                  (values (if (= (ldb (byte 2 0) significand) #b11)
                              s+1
                              s)
                          k
                          sign))
                 (:to-even ; ?11 or 110
                  (values (if (and (logbitp 1 significand)
                                   (or (logbitp 0 significand)
                                       (logbitp 2 significand)))
                              s+1
                              s)
                          k
                          sign))
                 (otherwise ; ?11 or 010
                  (values (if (and (logbitp 1 significand)
                                   (or (logbitp 0 significand)
                                       (not (logbitp 2 significand))))
                              s+1
                              s)
                          k
                          sign))))))))))

#+clisp
(defmethod quaviver:float-triple ((client client) (base (eql 10)) value)
  (declare (optimize speed))
  (typecase value
    #+quaviver/short-float
    (short-float
     (%schubfach client short-float value))
    (single-float
     (%schubfach client single-float value))
    (double-float
     (%schubfach client double-float value))
    #+quaviver/long-float
    (long-float
     (%schubfach client long-float value))
    (otherwise
     (call-next-method))))

#+(and (not clisp) quaviver/short-float)
(defmethod quaviver:float-triple ((client client) (base (eql 10)) (value short-float))
  (declare (optimize speed))
  (%schubfach client short-float value))

#-clisp
(defmethod quaviver:float-triple ((client client) (base (eql 10)) (value single-float))
  (declare (optimize speed))
  (%schubfach client single-float value))

#-clisp
(defmethod quaviver:float-triple ((client client) (base (eql 10)) (value double-float))
  (declare (optimize speed))
  (%schubfach client double-float value))

#+(and (not clisp) quaviver/long-float)
(defmethod quaviver:float-triple ((client client) (base (eql 10)) (value long-float))
  (declare (optimize speed))
  (%schubfach client long-float value))
