(in-package #:quaviver/stream)

(defun write-number (value stream
                     &key (base 10) (style :common-lisp))
  (quaviver:write-number (getf *clients* style) base value stream))
