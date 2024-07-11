(in-package #:quaviver/liebler)

(defclass client ()
  ())

(defmacro %liebler (client result-type significand exponent sign)
  (with-accessors ((arithmetic-size quaviver:arithmetic-size)
                   (significand-size quaviver:significand-size)
                   (min-exponent quaviver:min-exponent)
                   (max-exponent quaviver:max-exponent))
      result-type
    (let ((word-size (+ 6 significand-size)))
      `(locally
           (declare (type (unsigned-byte ,word-size)
                          ,significand)
                    (type (or fixnum keyword) ,exponent)
                    (type fixnum ,sign)
                    (optimize speed))
         (if (or (not (numberp ,exponent))
                 (zerop ,significand))
             (quaviver:integer-float ,client ',result-type 2
                                     ,significand ,exponent ,sign)
             (let* ((k (quaviver.math:floor-log-expt 2 10 ,exponent))
                    (q (+ k (integer-length ,significand) ,(- significand-size)))
                    (shift (- ,(+ significand-size 2) (integer-length ,significand))))
               (declare (type fixnum k shift))
               ;; The following overflow and underflow checks are not
               ;; strict checks. Stricter checks will happen in
               ;; integer-float/2. These are here to protect the expt10
               ;; table lookup from an out of bounds error.
               (when (> q ,(+ max-exponent
                              (quaviver.math:ceiling-log-expt 2 10 1)))
                 (quaviver::integer-float-overflow
                  ,client ',result-type 10 significand exponent sign))
               (when (< q ,(- min-exponent
                              (quaviver.math:ceiling-log-expt 2 10 1)))
                 (quaviver::integer-float-underflow
                  ,client ',result-type 10 significand exponent sign))
               (setf ,significand (quaviver.math:round-to-odd
                                   ,arithmetic-size
                                   (ash ,significand shift)
                                   (quaviver.math:expt ,arithmetic-size 10
                                                       (- ,exponent)))
                     k (- k -1 shift)
                     shift (- ,significand-size (integer-length ,significand)))
               (quaviver:integer-float ,client ',result-type 2
                                       (round ,significand (ash 1 (- shift)))
                                       (- k shift)
                                       ,sign)))))))

#-clisp
(defmethod quaviver:integer-float
    ((client client) (result-type (eql 'single-float)) (base (eql 10)) significand exponent sign)
  (%liebler client single-float
            significand exponent sign))

#-clisp
(defmethod quaviver:integer-float
    ((client client) (result-type (eql 'double-float)) (base (eql 10)) significand exponent sign)
  (%liebler client double-float
            significand exponent sign))

#+(and (not clisp) quaviver/long-float)
(defmethod quaviver:integer-float
    ((client client) (result-type (eql 'long-float)) (base (eql 10)) significand exponent sign)
  (%liebler client long-float
            significand exponent sign))
