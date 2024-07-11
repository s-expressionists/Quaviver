(in-package #:quaviver/liebler)

(defclass client ()
  ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %compute-min-exponent (min-exponent significand-size)
    (declare (ignore significand-size))
    (ceiling (/ (* (- min-exponent 4) 4194304) 13933176)))

  (defun %compute-max-exponent (max-exponent significand-size)
    (floor (/ (* (+ max-exponent 4 significand-size) 4194304) 13933176))))

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
         (cond ((or (not (numberp ,exponent))
                    (zerop ,significand))
                (quaviver:integer-float ,client ',result-type 2
                                        ,significand ,exponent ,sign))
               ((> ,exponent ,(%compute-max-exponent max-exponent significand-size))
                (quaviver::integer-float-overflow
                 ,client ',result-type 10 ,significand ,exponent ,sign))
               ((< ,exponent ,(%compute-min-exponent min-exponent significand-size))
                (quaviver::integer-float-underflow
                 ,client ',result-type 10 ,significand ,exponent ,sign))
               (t
                (let* ((k (quaviver.math:floor-log-expt 2 10 ,exponent))
                       (q (+ k (integer-length ,significand) ,(- significand-size)))
                       (shift (- ,word-size (integer-length ,significand))))
                  (declare (type fixnum k shift))
                  ;; The following overflow and underflow checks are not
                  ;; strict checks. Stricter checks will happen in
                  ;; integer-float/2. These are here to protect the expt10
                  ;; table lookup from an out of bounds error.
                  (when (> q ,(+ max-exponent
                                 (quaviver.math:ceiling-log-expt 2 10 1)))
                    (quaviver::integer-float-overflow
                     ,client ',result-type 10 ,significand ,exponent ,sign))
                  (when (< q ,(- min-exponent
                                 (quaviver.math:ceiling-log-expt 2 10 1)))
                    (quaviver::integer-float-underflow
                     ,client ',result-type 10 ,significand ,exponent ,sign))
                  (setf ,significand (quaviver.math:round-to-odd
                                      ,arithmetic-size
                                      (ash ,significand shift)
                                      (quaviver.math:expt ,arithmetic-size 10
                                                          (- ,exponent)))
                        k (- k -1 shift)
                        shift (- ,significand-size (integer-length ,significand)))
                  ,(quaviver:integer-float-form
                    nil result-type 2
                    `(round ,significand (ash 1 (- shift))) `(- k shift) 'sign))))))))

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
