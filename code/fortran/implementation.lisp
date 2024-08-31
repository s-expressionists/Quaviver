(in-package #:quaviver/fortran)

(defclass client () ())

(quaviver:define-number-parser
    (:client client :ratio nil :base 10 :case t)
    ()
  (:alternate? #\+
               (:sequence? #\-
                           (:set :sign -1)))
  (:alternate (:sequence #\.
                         (:set :integer nil)
                         (:digits :fractional))
              (:sequence (:digits :integral)
                         (:sequence? #\.
                                     (:set :integer nil)
                                     (:digits? :fractional))))
  (:sequence? (:assert :float t)
              (:alternate (:sequence #\E
                                     (:set :float-type 'single-float))
                          (:sequence #\D
                                     (:set :float-type 'double-float)))
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

#+clisp
(defmethod quaviver:write-number ((client client) (base (eql 10)) (value float) stream)
  (multiple-value-bind (significand exponent sign)
      (quaviver:float-triple client base value)
    (when (keywordp exponent)
      (error "Unable to represent ~a." exponent))
    (when (minusp sign)
      (write-char #\- stream))
    (quaviver:write-digits base significand stream)
    (write-char (etypecase value
                  (single-flaot #\E)
                  (double-float #\D))
                stream)
    (when (minusp exponent)
      (write-char #\- stream))
    (quaviver:write-digits base (abs exponent) stream))
  value)

#-clisp
(defmethod quaviver:write-number ((client client) (base (eql 10)) (value single-float) stream)
  (multiple-value-bind (significand exponent sign)
      (quaviver:float-triple client base value)
    (when (keywordp exponent)
      (error "Unable to represent ~a." exponent))
    (when (minusp sign)
      (write-char #\- stream))
    (quaviver:write-digits base significand stream)
    (write-char #\E stream)
    (when (minusp exponent)
      (write-char #\- stream))
    (quaviver:write-digits base (abs exponent) stream))
  value)

#-clisp
(defmethod quaviver:write-number ((client client) (base (eql 10)) (value double-float) stream)
  (multiple-value-bind (significand exponent sign)
      (quaviver:float-triple client base value)
    (when (keywordp exponent)
      (error "Unable to represent ~a." exponent))
    (when (minusp sign)
      (write-char #\- stream))
    (quaviver:write-digits base significand stream)
    (write-char #\D stream)
    (when (minusp exponent)
      (write-char #\- stream))
    (quaviver:write-digits base (abs exponent) stream))
  value)
