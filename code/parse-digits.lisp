(in-package #:quaviver)

(defun quaviver:parse-digits
    (base string
     &key (start 0) (end (length string))
          limit ignore)
  (prog ((result 0)
         (count 0)
         (leading-zero 0)
         (discarded 0)
         (ignored 0)
         digit)
   next
     (when (< start end)
       (when (find (elt string start) ignore)
         (incf start)
         (incf ignored)
         (go next))
       (setf digit (digit-char-p (elt string start) base))
       (cond ((null digit)
              (go terminate))
             ((or (null limit)
                  (< count limit))
              (setf result (+ (* result base) digit))
              (if (zerop result)
                  (incf leading-zero)
                  (incf count)))
             (t
              (incf discarded)))
       (incf start)
       (go next))
   terminate
     (return (values result start count leading-zero discarded ignored))))
