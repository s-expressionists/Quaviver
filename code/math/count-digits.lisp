(in-package #:quaviver.math)

(declaim (ftype (function ((integer 2 36) unsigned-byte) (unsigned-byte 22))
                count-digits)
         (inline count-digits))

(defvar *count-digits-table*
  (compute-count-digits +min-base+ +max-base+
                        (quaviver:arithmetic-size #+quaviver/long-float
                                                  'long-float
                                                  #-quaviver/long-float
                                                  'double-float)))

(defun count-digits (base value)
  (declare (optimize speed))
  (let ((len (integer-length value)))
    (cond ((zerop len)
           1)
          ((eql base 2)
           len)
          ((= (logcount base) 1)
           (values (ceiling len
                            (1- (integer-length base)))))
          (t
           (multiple-value-bind (count farp)
               (ceiling-log-expt/far base 2 (1- len))
             (if (or farp
                     (< value
                        (let ((table (svref *count-digits-table* (- base +min-base+))))
                          (if (< count (length table))
                              (svref table count)
                              (cl:expt base count)))))
                 count
                 (1+ count)))))))

(define-compiler-macro count-digits (&whole whole base value)
  (cond ((not (constantp base))
         whole)
        ((eql base 2)
         `(integer-length ,value))
        ((= (logcount base) 1)
         `(values (ceiling (integer-length ,value)
                           ,(1- (integer-length base)))))
        (t
         (let ((table (svref *count-digits-table* (- base +min-base+))))
           `(let* ((value ,value)
                   (len (integer-length value)))
              (declare (optimize speed))
              (if (zerop len)
                  1
                  (multiple-value-bind (count farp)
                      (ceiling-log-expt/far ,base 2 (1- len))
                    (declare (type quaviver:exponent-word count))
                    (if (or farp
                            (if (< count ,(length table))
                                (< value (svref ,table count))
                                (< value (cl:expt ,base count))))
                        count
                        (1+ count)))))))))
