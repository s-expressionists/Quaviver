(in-package #:quaviver/string)

(defclass common-lisp-client-impl
    (common-lisp-client
     quaviver/liebler:client
     quaviver/schubfach:client)
  ())

(defvar *clients*
  (list :common-lisp (make-instance 'common-lisp-client-impl)))

(defun parse-float (string
                    &key start end (radix 10)
                         ((:exponent-characters *exponent-characters*) nil exponent-characters-p)
                         (result-type *read-default-float-format*)
                         junk-allowed (format :common-lisp))
  (check-type string string)
  (setf end (or end (length string)))
  (multiple-value-bind (result pos)
      (quaviver:sequence-float (getf *clients* format)
                               result-type
                               radix
                               string
                               (or start 0)
                               end)
    (unless (or junk-allowed
                (= pos end))
      (error "junk found"))
    (values result pos)))
