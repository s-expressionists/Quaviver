(in-package #:quaviver)

(defmethod digits-integer (client base (digits vector))
  (declare (ignore client))
  (loop with result = 0
        for digit across digits
        finally (return result)
        do (setf result (+ (* result base) digit))))

(defmethod digits-integer (client (base (eql 2)) (digits vector))
  (declare (ignore client))
  (loop with result = 0
        for digit across digits
        finally (return result)
        do (setf result (logior (ash result 1) digit))))

(defmethod digits-integer (client base (digits string))
  (declare (ignore client))
  (loop with result = 0
        for digit across digits
        finally (return result)
        do (setf result (+ (* result base) (digit-char-p digit base)))))

(defmethod digits-integer (client (base (eql 2)) (digits string))
  (declare (ignore client))
  (loop with result = 0
        for digit across digits
        finally (return result)
        do (setf result (logior (ash result 1)
                                (if (eql digit #\1) 1 0)))))

(defmethod digits-integer (client base (digits list))
  (declare (ignore client))
  (loop with result = 0
        for digit in digits
        finally (return result)
        do (setf result (+ (* result base) digit))))

(defmethod digits-integer (client (base (eql 2)) (digits list))
  (declare (ignore client))
  (loop with result = 0
        for digit in digits
        finally (return result)
        do (setf result (logior (ash result 1) digit))))
