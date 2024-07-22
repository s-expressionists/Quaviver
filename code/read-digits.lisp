(in-package #:quaviver)

(defun read-digits (base stream &key limit ignore)
  (prog ((result 0)
         (count 0)
         (discarded 0)
         (ignored 0)
         (leading-zero 0)
         char
         digit
         (is-empty t))
   next
     (setf char (read-char stream nil))
     (unless char
       (go terminate))
     (when (find char ignore)
       (incf ignored)
       (go next))
     (setf digit (digit-char-p char base))
     (cond ((null digit)
            (unread-char char stream)
            (go terminate))
           ((or (null limit)
                (< count limit))
            (setf result (+ (* result base) digit)
                  is-empty nil)
            (if (zerop result)
                (incf leading-zero)
                (incf count)))
           (t
            (incf discarded)))
     (go next)
   terminate
     (return (values result
                     (if is-empty 0 count)
                     leading-zero
                     discarded
                     ignored))))
