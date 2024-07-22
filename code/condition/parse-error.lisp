(in-package #:quaviver.condition)

(define-condition assertion-failed-error (parse-error)
  ((message :reader assertion-failed-error-message
            :initarg :message
            :initform nil))
  (:report (lambda (condition stream)
             (format stream "Parsing assertion failed~@[: ~a~]."
                     (assertion-failed-error-message condition)))))

(define-condition invalid-character-error (parse-error)
  ((found :reader invalid-character-error-found
          :initarg :found
          :initform nil)
   (expected :reader invalid-character-error-expected
             :initarg :expected
             :initform nil))
  (:report (lambda (condition stream)
             (format stream "Expected ~:[end of input~;~:*~s~] but found ~:[end of input~;~:*~s~] instead."
                     (invalid-character-error-expected condition)
                     (invalid-character-error-found condition)))))

(define-condition invalid-leading-zeros-error (parse-error)
  ((part :reader invalid-leading-zeros-error-part
         :initarg :part
         :initform nil)
   (count :reader invalid-leading-zeros-error-count
          :initarg :count
          :initform 0))
  (:report (lambda (condition stream)
             (format stream "Found ~d invalid leading zero~p in the ~(~a~)."
                     (invalid-leading-zeros-error-count condition)
                     (invalid-leading-zeros-error-count condition)
                     (invalid-leading-zeros-error-part condition)))))

(define-condition invalid-property-error (parse-error)
  ((name :reader invalid-property-error-name
         :initarg :name
         :initform nil)
   (value :reader invalid-property-error-value
          :initarg :value
          :initform nil)
   (expected :reader invalid-property-error-expected
             :initarg :expected
             :initform nil))
  (:report (lambda (condition stream)
             (format stream "Expected property ~s to have the value ~s but it has the value of ~s instead."
                     (invalid-property-error-name condition)
                     (invalid-property-error-expected condition)
                     (invalid-property-error-value condition)))))

(define-condition missing-digits-error (parse-error)
  ((part :reader missing-digits-error-part
         :initarg :part
         :initform nil)
   (found :reader missing-digits-error-found
          :initarg :found
          :initform nil))
  (:report (lambda (condition stream)
             (format stream "Expected digits~@[ for ~(~a~)~] but found ~:[end of input~;~:*~s~] instead."
                     (missing-digits-error-part condition)
                     (missing-digits-error-found condition)))))
