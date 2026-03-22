(cl:in-package #:quaviver)

(defun unique-name (&rest args)
  (gensym (apply #'concatenate 'string
                 (mapcar (lambda (arg)
                           (typecase arg
                             (symbol (symbol-name arg))
                             (string arg)
                             (otherwise (write-to-string arg))))
                         args))))

#+(or)(define-compiler-macro (&whole form &rest args)
  (if (and (consp name)
           (eq (car name) 'quote)
           (symbolp (cadr name)))
      `(gensym ,(symbol-name (cadr name)))
      form))

(defmacro with-unique-names (names &body body)
  `(let ,(mapcar (lambda (name)
                   `(,name (unique-name ',name)))
                 names)
     ,@body))
