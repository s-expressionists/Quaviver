(in-package #:quaviver)

(defmethod digits-integer (client base (digits sequence) part
                           &optional (start 0) (end (length digits))
                             limit)
  (prog ((result 0)
         (count 0)
         (discarded 0)
         digit)
   next
     (when (< start end)
       (setf digit (digit-integer client base (elt digits start) part count))
       (cond ((null digit)
              (go terminate))
             ((eq digit :skip))
             ((or (null limit)
                  (< count limit))
              (setf result (+ (* result base) digit))
              (unless (zerop result)
                (incf count)))
             (t
              (incf discarded)))
       (incf start)
       (go next))
   terminate
     (return (values result start count discarded))))
