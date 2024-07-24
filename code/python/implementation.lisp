(in-package #:quaviver/python)

(defclass client ()
  ((%version :accessor version
             :initarg :version
             :initform "3.6")
   (%binary-grouping :accessor binary-grouping
                     :initarg :binary-grouping
                     :initform #(4))
   (%octal-grouping :accessor octal-grouping
                    :initarg :octal-grouping
                    :initform #(4))
   (%hexadecimal-grouping :accessor hexadecimal-grouping
                          :initarg :hexadecimal-grouping
                          :initform #(4))
   (%decimal-grouping :accessor decimal-grouping
                      :initarg :decimal-grouping
                      :initform #(-3 3))
   (%exponent-grouping :accessor exponent-grouping
                       :initarg :exponent-grouping
                       :initform #(3))))

(defun digit-grouping-p (client)
  (uiop:version<= "3.6" (version client)))

(defun digit-separators (client)
  (if (digit-grouping-p client)
      "_"
      ""))

(defun group-marker (client)
  (when (digit-grouping-p client)
    #\_))

(quaviver:define-number-parser
    (:client client :ratio nil :float-type 'double-float)
    (:exponent? (:sequence? (:assert :float t)
                            #\e
                            (:set :integer nil)
                            (:alternate? #\+
                                         (:sequence #\-
                                                    (:set :exponent-sign -1)))
                            (:digits :exponent
                                     :ignore 'digit-separators)))
  (:alternate? #\+
               (:sequence #\-
                          (:set :sign -1)))
  (:alternate (:sequence #\0
                         (:alternate (:sequence (:assert :integer t)
                                                #\b
                                                (:set :integral-base 2
                                                      :float nil)
                                                (:digits :integral
                                                         :ignore 'digit-separators))
                                     (:sequence (:assert :integer t)
                                                #\x
                                                (:set :integral-base 16
                                                      :float nil)
                                                (:digits :integral
                                                         :ignore 'digit-separators))
                                     (:sequence (:assert :integer t)
                                                #\o
                                                (:set :integral-base 8
                                                      :float nil)
                                                (:digits :integral
                                                 :ignore 'digit-separators))))
              (:sequence (:assert :float t)
                         #\.
                         (:set :integer nil)
                         (:digits :fractional
                                   :ignore 'digit-separators)
                         :exponent?)
              (:sequence (:digits :integral
                                  :leading-zeros nil
                                  :ignore 'digit-separators)
                         (:sequence? (:assert :float t)
                                     #\.
                                     (:set :integer nil)
                                     (:digits? :fractional
                                      :ignore 'digit-separators))
                         :exponent?)))

(defmethod quaviver:write-number ((client client) (base (eql 2)) (value integer) stream)
  (when (minusp value)
    (write-char #\- stream))
  (write-string "0b" stream)
  (quaviver:write-digits base (abs value) stream
                         :digit-grouping (binary-grouping client)
                         :group-marker (group-marker client))
  value)

(defmethod quaviver:write-number ((client client) (base (eql 8)) (value integer) stream)
  (when (minusp value)
    (write-char #\- stream))
  (write-string "0o" stream)
  (quaviver:write-digits base (abs value) stream
                         :digit-grouping (octal-grouping client)
                         :group-marker (group-marker client))
  value)

(defmethod quaviver:write-number ((client client) (base (eql 10)) (value integer) stream)
  (when (minusp value)
    (write-char #\- stream))
  (quaviver:write-digits base (abs value) stream
                         :digit-grouping (decimal-grouping client)
                         :group-marker (group-marker client))
  value)

(defmethod quaviver:write-number ((client client) (base (eql 16)) (value integer) stream)
  (when (minusp value)
    (write-char #\- stream))
  (write-string "0x" stream)
  (quaviver:write-digits base (abs value) stream
                         :digit-grouping (hexadecimal-grouping client)
                         :group-marker (group-marker client))
  value)

(defmethod quaviver:write-number ((client client) (base (eql 10)) (value float) stream)
  (multiple-value-bind (significand exponent sign)
      (quaviver:float-integer client base value)
    (when (keywordp exponent)
      (error "Unable to represent ~a." exponent))
    (when (minusp sign)
      (write-char #\- stream))
    (let ((len (quaviver.math:ceiling-log-expt 10 2 (integer-length significand))))
      (cond ((<= (- len) exponent -1)
             (quaviver:write-digits base significand stream
                                    :fractional-position (+ len exponent)
                                    :fractional-marker #\.
                                    :digit-grouping (decimal-grouping client)
                                    :group-marker (group-marker client)))
            (t
             (quaviver:write-digits base significand stream
                                    :digit-grouping (decimal-grouping client)
                                    :group-marker (group-marker client))
             (unless (zerop exponent)
               (write-char #\e stream)
               (when (minusp exponent)
                 (write-char #\- stream))
               (quaviver:write-digits base (abs exponent) stream
                                      :digit-grouping (exponent-grouping client)
                                      :group-marker (group-marker client)))))))
  value)
