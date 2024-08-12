(in-package #:quaviver/common-lisp)

(declaim (ftype (function (float) base-char) exponent-marker)
         (inline exponent-marker))

(defun exponent-marker (value)
  (if (typep value *read-default-float-format*)
      #+(or ccl lispworks) #\E #-(or ccl lispworks) #\e
      (etypecase value
        (short-float  #+(or ccl lispworks) #\S #-(or ccl lispworks) #\s)
        (single-float #+(or ccl lispworks) #\F #-(or ccl lispworks) #\f)
        (double-float #+(or ccl lispworks) #\D #-(or ccl lispworks) #\d)
        (long-float   #+(or ccl lispworks) #\L #-(or ccl lispworks) #\l))))

(defclass client ()
  ((%extended-exponent-sign :accessor extended-exponent-sign-p
                            :initarg :extended-exponent-sign
                            :initform #+ccl t #-ccl nil)))

(quaviver:define-number-parser
    (:client client)
    (:exponent? (:sequence? (:assert :float t)
                            (:alternate #\e
                                        (:sequence #\s
                                                   (:set :float-type 'short-float))
                                        (:sequence #\f
                                                   (:set :float-type 'single-float))
                                        (:sequence #\d
                                                   (:set :float-type 'double-float))
                                        (:sequence #\l
                                                   (:set :float-type 'long-float)))
                            (:set :integer nil :ratio nil)
                            (:sequence? (:assert (complement #'extended-exponent-sign-p))
                                        (:alternate #\+
                                                    (:sequence #\-
                                                               (:set :exponent-sign -1))))
                            (:sequence? (:assert 'extended-exponent-sign-p)
                                        (:alternate #\+
                                                    (:sequence #\-
                                                               (:set :exponent-sign -1)))
                                        (:alternate? (:sequence #\+
                                                                (:set :code :infinity))
                                                     (:sequence #\-
                                                                (:set :code :quiet-nan))))
                            (:digits :exponent))
     :sign? (:alternate? #\+
                         (:sequence #\-
                                    (:set :sign -1))))
  (:alternate (:sequence #\#
                         (:alternate (:sequence #\b
                                                (:set :integral-base 2
                                                      :divisor-base 2))
                                     (:sequence #\o
                                                (:set :integral-base 8
                                                      :divisor-base 8))
                                     (:sequence #\x
                                                (:set :integral-base 16
                                                      :divisor-base 16)))
                         :sign?
                         (:digits :integral)
                         (:sequence? (:assert :ratio t)
                                     #\/
                                     (:set :integer nil :float nil)
                                     (:digits :divisor)))
              (:sequence :sign?
                         (:alternate (:sequence (:assert :float t)
                                                #\.
                                                (:digits :fractional)
                                                (:set :ratio nil
                                                      :integer nil)
                                                :exponent?)
                                     (:sequence (:digits :integral)
                                                (:alternate (:sequence (:assert :ratio t)
                                                                       #\/
                                                                       (:set :integer nil :float nil)
                                                                       (:digits :divisor))
                                                            (:sequence (:sequence? #\.
                                                                                   (:sequence? (:assert :float t)
                                                                                               (:digits :fractional)
                                                                                               (:set :ratio nil
                                                                                                     :integer nil)))
                                                             :exponent?)))))))

(defmethod quaviver:write-number ((client client) (base (eql 2)) (value integer) stream)
  (write-string "#b" stream)
  (when (minusp value)
    (write-char #\- stream))
  (quaviver:write-digits base (abs value) stream)
  value)

(defmethod quaviver:write-number ((client client) (base (eql 8)) (value integer) stream)
  (write-string "#o" stream)
  (when (minusp value)
    (write-char #\- stream))
  (quaviver:write-digits base (abs value) stream)
  value)

(defmethod quaviver:write-number ((client client) (base (eql 10)) (value integer) stream)
  (when (minusp value)
    (write-char #\- stream))
  (quaviver:write-digits base (abs value) stream)
  value)

(defmethod quaviver:write-number ((client client) (base (eql 16)) (value integer) stream)
  (write-string "#x" stream)
  (when (minusp value)
    (write-char #\- stream))
  (quaviver:write-digits base (abs value) stream)
  value)

(defmethod quaviver:write-number ((client client) base (value integer) stream)
  (unless (<= base 36)
    (error "Invalid base ~a" base))
  (write-char #\# stream)
  (quaviver:write-digits 10 base stream)
  (write-char #\r stream)
  (when (minusp value)
    (write-char #\- stream))
  (quaviver:write-digits base (abs value) stream)
  value)

(defmethod quaviver:write-number ((client client) base (value float) stream)
  (declare (ignore base))
  (multiple-value-bind (significand exponent sign)
      (quaviver:float-triple client 10 value)
    (cond ((and (extended-exponent-sign-p client)
                (eq exponent :infinity))
           (when (minusp sign)
             (write-char #\- stream))
           (write-char #\1 stream)
           (write-char (exponent-marker value) stream)
           (write-string "++0" stream))
          ((and (extended-exponent-sign-p client)
                (eq exponent :quiet-nan))
           (when (minusp sign)
             (write-char #\- stream))
           (write-char #\1 stream)
           (write-char (exponent-marker value) stream)
           (write-string "+-0" stream))
          ((keywordp exponent)
           (error "Unable to represent ~a." exponent))
          (t
           (when (minusp sign)
             (write-char #\- stream))
           (let ((len (quaviver.math:ceiling-log-expt 10 2 (integer-length significand))))
             (cond ((and (typep value *read-default-float-format*)
                         (<= (- len) exponent -1))
                    (quaviver:write-digits 10 significand stream
                                           :fractional-position (+ len exponent)
                                           :fractional-marker #\.))
                   (t
                    (quaviver:write-digits 10 significand stream)
                    (write-char (exponent-marker value) stream)
                    (when (minusp exponent)
                      (write-char #\- stream))
                    (quaviver:write-digits 10 (abs exponent) stream)))))))
  value)
