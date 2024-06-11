(cl:in-package #:quaviver/benchmark)

(defvar *client-initargs*
  '((quaviver/burger-dybvig:client-1)
    (quaviver/burger-dybvig:client-2)
    (quaviver/schubfach:client)))
(defun float-decimal ()
  (labels ((bench (title limit)
             (write-string (cl-spark:vspark
                            (mapcar (lambda (initargs
                                             &aux (client (apply #'make-instance initargs)))
                                      (the-cost-of-nothing:benchmark (quaviver:float-decimal client
                                                                                             (random limit))))
                                    *client-initargs*)
                            :title title
                            :min 0
                            :size 132
                            :labels (mapcar (lambda (initargs
                                                     &aux (*print-case* :downcase))
                                              (format nil "~{~s~^ ~}" initargs))
                                            *client-initargs*)))))
    (bench "float-decimal single-float" most-positive-single-float)
    (terpri)
    (bench "float-decimal double-float" most-positive-double-float)))
