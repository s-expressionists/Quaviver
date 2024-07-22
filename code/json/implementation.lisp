(in-package #:quaviver/json)

(defclass client () ())

(quaviver:define-number-parser
    (:client client :ratio nil :base 10)
    ()
  (:sequence? #\-
              (:set :sign -1))
  (:digits :integral
           :leading-zeros nil)
  (:sequence? (:assert :float t)
              #\.
              (:digits :fractional)
              (:set :integer nil))
  (:sequence? (:assert :float t)
              #\e
              (:set :integer nil)
              (:alternate? #\+
                           (:sequence? #\-
                                       (:set :exponent-sign -1)))
              (:digits :exponent)))

(defmethod quaviver:write-number ((client client) base (value integer) stream)
  (cond ((minusp value)
         (write-char #\- stream)
         (quaviver:write-digits base (- value) stream))
        (t
         (quaviver:write-digits base value stream))))

(defmethod quaviver:write-number ((client client) base (value float) stream)
  (multiple-value-bind (significand exponent sign)
      (quaviver:float-integer client base value)
    (when (keywordp exponent)
      (error "Unable to represent ~a in JSON." exponent))
    (when (minusp sign)
      (write-char #\- stream))
    (let ((len (quaviver.math:ceiling-log-expt 10 2 (integer-length significand))))
      (cond ((<= (- len) exponent -1)
             (when (eql (- len) exponent)
               (write-char #\0 stream))
             (quaviver:write-digits base significand stream
                                    :fractional-position (+ len exponent)
                                    :fractional-marker #\.))
            (t
             (quaviver:write-digits base significand stream)
             (unless (zerop exponent)
               (write-char #\e stream)
               (quaviver:write-number client base exponent stream)))))))
