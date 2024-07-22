(in-package #:quaviver/c)

(deftype c-c++-standard ()
  '(member :c89 :c99 :c11 :c17 :c23
           :c++98 :c++03 :c++11 :c++14 :c++17 :c++20 :c++23 :c++26))

(defclass client ()
  ((%standard :accessor client-standard
              :initarg :standard
              :initform :c++23
              :type c-c++-standard)
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
  (and (member (client-standard client) '(:c23 :c++14 :c++17 :c++20 :c++23 :c++26))
       t))

(defun digit-separators (client)
  (if (digit-grouping-p client)
      "'"
      ""))

(defun group-marker (client)
  (when (digit-grouping-p client)
    #\'))

(defun binary-integer-literals-p (client)
  (and (member (client-standard client) '(:c23 :c++14 :c++17 :c++20 :c++23 :c++26))
       t))

(defun hexadecimal-float-literals-p (client)
  (and (member (client-standard client) '(:c++17 :c++20 :c++23 :c++26))
       t))

(defun long-long-suffix-p (client)
  (and (member (client-standard client) '(:c++11 :c++17 :c++20 :c++23 :c++26))
       t))

(defun size-suffix-p (client)
  (and (member (client-standard client) '(:c++23 :c++26))
       t))

(defun float-size-suffix-p (client)
  (and (member (client-standard client) '(:c++23 :c++26))
       t))

(quaviver:define-number-parser
    (:client client :ratio nil :float-type 'double-float)
    ()
  (:alternate? #\+
               (:sequence #\-
                          (:set :sign -1)))
  (:alternate (:sequence #\0
                         (:alternate (:sequence (:assert 'binary-integer-literals-p)
                                                (:assert :integer t)
                                                #\b
                                                (:set :integral-base 2
                                                      :float nil)
                                                (:digits :integral
                                                         :ignore 'digit-separators))
                                     (:sequence #\x
                                                (:set :integral-base 16
                                                      :fractional-base 16
                                                      :exponent-base 10
                                                      :base 2)
                                                (:digits :integral
                                                         :ignore 'digit-separators)
                                                (:sequence? (:assert 'hexadecimal-float-literals-p)
                                                            (:assert :float t)
                                                            #\.
                                                            (:set :integer nil)
                                                            (:digits? :fractional
                                                             :ignore 'digit-separators))
                                                (:sequence? (:assert 'hexadecimal-float-literals-p)
                                                            (:assert :float t)
                                                            #\p
                                                            (:set :integer nil)
                                                            (:alternate? #\+
                                                                         (:sequence #\-
                                                                                    (:set :exponent-sign -1)))
                                                            (:digits :exponent
                                                                     :ignore 'digit-separators)))
                                     (:sequence (:assert :integer t)
                                                (:set :integral-base 8
                                                      :float nil)
                                                (:digits :integral
                                                         :ignore 'digit-separators))))
              (:sequence (:digits :integral
                                  :leading-zeros nil
                                  :ignore 'digit-separators)
                         (:sequence? (:assert :float t)
                                     #\.
                                     (:set :integer nil)
                                     (:digits? :fractional
                                               :ignore 'digit-separators))
                         (:sequence? (:assert :float t)
                                     #\e
                                     (:set :integer nil)
                                     (:alternate? #\+
                                                  (:sequence #\-
                                                             (:set :exponent-sign -1)))
                                     (:digits :exponent
                                              :ignore 'digit-separators))
                         (:sequence? (:assert :integer t)
                                     (:set :float nil))))
  (:alternate? (:sequence (:assert :float t)
                          #\f
                          (:set :float-type 'single-float)
                          (:sequence? (:assert 'float-size-suffix-p)
                                      (:alternate? (:sequence #\1
                                                              (:alternate (:sequence #\6
                                                                                     #+quaviver/short-float
                                                                                     (:set :float-type 'short-float))
                                                                          (:sequence "28"
                                                                                     (:set :float-type
                                                                                           #+quaviver/long-float
                                                                                           'long-float
                                                                                           #-quaviver/long-float
                                                                                           'double-float))))
                                                   (:sequence "32")
                                                   (:sequence "64"
                                                              (:set :float-type 'double-float)))))
               (:sequence (:assert :float t)
                          #\l
                          #+quaviver/long-float
                          (:set :float-type 'long-float))
               (:sequence (:assert 'float-size-suffix-p)
                          (:assert :float t)
                          "bf16"
                          (:set :float-type
                                #+quaviver/short-float 'short-float
                                #-quaviver/short-float 'single-float))
               (:sequence (:assert :integer t)
                          #\l
                          (:sequence? (:assert 'long-long-suffix-p)
                                      #\l)
                          (:sequence? #\u))
               (:sequence (:assert 'size-suffix-p)
                          (:assert :integer t)
                          #\z
                          (:sequence? #\u))
               (:sequence (:assert :integer t)
                          #\u
                          (:alternate? (:sequence #\l
                                                  (:sequence? (:assert 'long-long-suffix-p)
                                                              #\l))
                                       (:sequence (:assert 'size-suffix-p)
                                                  #\z)))))

(defmethod quaviver:write-number ((client client) (base (eql 2)) (value integer) stream)
  (unless (binary-integer-literals-p client)
    (error "Cannot print base 2 integers in the ~a standard." (client-standard client)))
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
  (write-char #\0 stream)
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

(defun write-float-suffix (client value stream)
  (declare (ignore client))
  (typecase value
    (single-float
     (write-char #\f stream))
    (long-float
     (write-char #\l stream))))

(defmethod quaviver:write-number ((client client) (base (eql 10)) (value float) stream)
  (multiple-value-bind (significand exponent sign)
      (quaviver:float-integer client base value)
    (when (keywordp exponent)
      (error "Unable to represent ~a in ~a." exponent (client-standard client)))
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
                                      :group-marker (group-marker client))))))
    (write-float-suffix client value stream))
  value)

(defmethod quaviver:write-number ((client client) (base (eql 16)) (value float) stream)
  (multiple-value-bind (significand exponent sign)
      (quaviver:float-integer client 2 value)
    (when (keywordp exponent)
      (error "Unable to represent ~a in ~a." exponent (client-standard client)))
    (when (minusp sign)
      (write-char #\- stream))
    (write-string "0x" stream)
    (quaviver:write-digits 16 significand stream
                           :digit-grouping (hexadecimal-grouping client)
                           :group-marker (group-marker client))
    (unless (zerop exponent)
      (write-char #\p stream)
      (when (minusp exponent)
        (write-char #\- stream))
      (quaviver:write-digits 10 (abs exponent) stream
                             :digit-grouping (exponent-grouping client)
                             :group-marker (group-marker client)))
    (write-float-suffix client value stream))
  value)
