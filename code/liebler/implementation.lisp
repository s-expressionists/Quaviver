(in-package #:quaviver/liebler)

(defclass client ()
  ())

(defmacro %liebler (client result-type significand exponent sign bits significand-size expt10 round-to-odd)
  `(if (or (not (numberp ,exponent))
           (zerop ,significand))
       (quaviver:integer-float ,client ,result-type 2
                               ,significand ,exponent ,sign)
       (let ((k (quaviver/math:floor-log2-expt10 ,exponent))
             (shift (- ,bits (integer-length ,significand))))
         (setf ,significand (,round-to-odd (,expt10 (- ,exponent))
                                           (ash ,significand shift))
               k (- k -1 shift)
               shift (- ,significand-size (integer-length ,significand)))
         (quaviver:integer-float ,client ,result-type 2
                                 (round ,significand (ash 1 (- shift)))
                                 (- k shift)
                                 ,sign))))

(defmethod quaviver:integer-float
    ((client client) (result-type (eql 'single-float)) (base (eql 10)) significand exponent sign)
  (%liebler client result-type
            significand exponent sign
            32 24
            quaviver/math:expt10/32
            quaviver/math:round-to-odd/32))

(defmethod quaviver:integer-float
    ((client client) (result-type (eql 'double-float)) (base (eql 10)) significand exponent sign)
  (%liebler client result-type
            significand exponent sign
            64 53
            quaviver/math:expt10/64
            quaviver/math:round-to-odd/64))

#+quaviver/long-float
(defmethod quaviver:integer-float
    ((client client) (result-type (eql 'long-float)) (base (eql 10)) significand exponent sign)
  (%liebler client result-type
            significand exponent sign
            128 64
            quaviver/math:expt10/128
            quaviver/math:round-to-odd/128))
