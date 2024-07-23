(in-package #:quaviver/blub)

(defclass client () ())

(quaviver:define-number-parser
    (:client client :ratio nil :base 10)
    ()
  (:alternate? #\+
               (:sequence? #\-
                           (:set :sign -1)))
  (:alternate (:sequence #\.
                         (:set :integer nil)
                         (:digits :fractional))
              (:sequence (:digits :integral)
                         (:sequence? #\.
                                     (:sequence? (:assert :float t)
                                                 (:digits :fractional)
                                                 (:set :integer nil)))))
  (:sequence? (:assert :float t)
              #\e
              (:set :integer nil)
              (:alternate? #\+
                           (:sequence? #\-
                                       (:set :exponent-sign -1)))
              (:digits :exponent)))

(defmethod quaviver:write-number ((client client) (base (eql 10)) (value integer) stream)
  (when (minusp value)
    (write-char #\- stream))
  (quaviver:write-digits base (abs value) stream)
  value)

(defmethod quaviver:write-number ((client client) (base (eql 10)) (value float) stream)
  (multiple-value-bind (significand exponent sign)
      (quaviver:float-integer client base value)
    (when (keywordp exponent)
      (error "Unable to represent ~a in ~a." exponent (client-standard client)))
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
               (when (minusp exponent)
                 (write-char #\- stream))
               (quaviver:write-digits base (abs exponent) stream))))))
  value)
