(in-package #:quaviver)

(defmethod digit-integer (client base (digit character) part count)
  (declare (ignore cient base part count))
  (digit-char-p digit base))

(defmethod digit-integer (client base (digit integer) part count)
  (declare (ignore cient base part count))
  (and (numberp digit)
       (< -1 digit base)
       digit))
