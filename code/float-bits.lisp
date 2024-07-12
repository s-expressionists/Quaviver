(in-package #:quaviver)

(defmacro %float-bits (float-type value)
  (float-bits-form float-type value))

#+clisp
(defmethod float-bits (value)
  (typecase value
    #+quaviver/short-float
    (short-float
     (%float-bits short-float value))
    (single-float
     (%float-bits single-float value))
    (double-float
     (%float-bits double-float value))
    #+quaviver/long-float
    (long-float
     (%float-bits long-float value))
    (otherwise
     (call-next-method))))

#+(and (not clisp) quaviver/short-float)
(defmethod float-bits ((value short-float))
  (%float-bits short-float value))

#-clisp
(defmethod float-bits ((value single-float))
  (%float-bits single-float value))

#-clisp
(defmethod float-bits ((value double-float))
  (%float-bits double-float value))

#+(and (not clisp) quaviver/long-float)
(defmethod float-bits ((value long-float))
  (%float-bits long-float value))
