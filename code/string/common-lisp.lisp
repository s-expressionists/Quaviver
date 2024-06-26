(in-package #:quaviver/string)

(defclass common-lisp-client ()
  ((exponent-prefixes :accessor exponent-prefixes
                      :initarg :exponent-prefixes
                      :initform #+ccl '(#\+ :infinity #\- :quiet-nan)
                                #-ccl nil)))

(defvar *exponent-characters* nil)

(defmethod quaviver:sequence-float
    ((client common-lisp-client) result-type base (sequence string)
     &optional (start 0) (end (length sequence)))
  (unless (or *exponent-characters*
              (> base 10))
    (loop for char across sequence
          do (case char
               ((#\e #\E)
                (loop-finish))
               ((#\s #\S)
                (setf result-type 'short-float)
                (loop-finish))
               ((#\f #\F)
                (setf result-type 'single-float)
                (loop-finish))
               ((#\d #\D)
                (setf result-type 'double-float)
                (loop-finish))
               ((#\l #\L)
                (setf result-type 'long-float)
                (loop-finish)))))
  (prog ((significand-sign 1)
         (significand-integer 0)
         (significand-fraction 0)
         (significand-fraction-digits 0)
         count discarded
         (exponent-sign 1)
         (exponent 0)
         (code nil)
         (limit (1+ (ceiling-log-expt base 2 (quaviver:significand-size result-type)))))
     (unless (< start end)
       (error "no float"))
     (case (char sequence start)
       (#\+
        (incf start))
       (#\-
        (setf significand-sign -1)
        (incf start)))
     (unless (< start end)
       (error "no float"))
     (multiple-value-setq (significand-integer start count discarded)
       (quaviver:digits-integer client base sequence :integer start (length sequence) limit))
     (decf limit count)
     (unless (< start end)
       (go assemble))
     (unless (char= (char sequence start) #\.)
       (go check-exponent))
     (incf start)
     (multiple-value-setq (significand-fraction start significand-fraction-digits)
       (quaviver:digits-integer client base sequence :fraction start (length sequence) limit))
   check-exponent
     (unless (< start end)
       (go assemble))
     (unless (find (char sequence start) (or *exponent-characters* "eEsSfFdDlL"))
       (go assemble))
     (incf start)
     (when (char= (char sequence start) #\+)
       (incf start)
       (when (setf code (getf (exponent-prefixes client) (char sequence start)))
         (incf start)))
     (when (char= (char sequence start) #\-)
       (setf exponent-sign -1)
       (incf start)
       (when (setf code (getf (exponent-prefixes client) (char sequence start)))
         (incf start)))
     (multiple-value-setq (exponent start)
       (quaviver:digits-integer client base sequence :exponent start))
     (incf exponent discarded)
   assemble
     (return (values (if code
                         (quaviver:integer-float client
                                                 result-type base
                                                 1
                                                 code
                                                 (if (eq code :infinity)
                                                     significand-sign
                                                     1))
                         (quaviver:integer-float client
                                                 result-type base
                                                 (+ (* significand-integer
                                                       (expt base significand-fraction-digits))
                                                    significand-fraction)
                                                 (* exponent-sign
                                                    (- exponent
                                                       significand-fraction-digits))
                                                 significand-sign))
                     start))))
