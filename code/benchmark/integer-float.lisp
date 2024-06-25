(cl:in-package #:quaviver/benchmark)

(defvar *integer-float-tests*
  (list `(:type single-float)
        `(:type double-float)
        #+quaviver/long-float
        `(:type long-float)))

(defvar *integer-float-clients*
  `((:label "Jaffer"
     :initargs (quaviver/jaffer:client)
     :types (single-float double-float long-float))
    (:label "Liebler"
     :initargs (quaviver/liebler:client)
     :types (single-float double-float long-float))))

(defvar *ieee754-client* (make-instance 'quaviver/ieee754:client))

(defvar *schubfach-client* (make-instance 'quaviver/schubfach:client))


(defun random-float (type)
  (multiple-value-list
   (quaviver:float-integer *schubfach-client* 10
                           (quaviver:bits-float *ieee754-client*
                                                type
                                                (random (ash 1 (quaviver:storage-size type)))))))

(defun integer-float (&key (base 10)
                           (name (uiop:implementation-identifier)))
  (labels ((bench (clients limit key
                   &aux (significand-limit (ash 1 (quaviver:significand-size key)))
                        (exponent-limit (1- (ash 1 (byte-size (quaviver:exponent-bytespec key)))))
                        (exponent-bias (quaviver:exponent-bias key)))
             (mapcar (lambda (properties
                              &aux (client (apply #'make-instance
                                                  (getf properties :initargs))))
                       (cond ((member key (getf properties :types))
                              ;; Do one conversion in case there is some initialization needed.
                               (apply #'quaviver:integer-float client key base (random-float key))                           (list* key
                                     (the-cost-of-nothing:benchmark
                                      (apply #'quaviver:integer-float client key base (random-float key)))
                                     properties))
                             (t
                              properties)))
                     clients))
           (plot (title results key)
             (write-string (cl-spark:vspark
                            (mapcan (lambda (properties
                                             &aux (value (getf properties key)))
                                      (when value
                                        (list value)))
                                    results)
                            :title title
                            :min 0
                            :size 132
                            :labels (mapcan (lambda (client)
                                              (when (getf client key)
                                                (list (getf client :label))))
                                            results)))))
    (let ((results *integer-float-clients*)
          (table (ascii-table:make-table
                  (list* "client"
                         (loop for test in *integer-float-tests*
                               for type = (getf test :type)
                               collect (format nil "~21@a"
                                               (format nil "absolute ~(~a~)" type))
                               collect (format nil "~21@a"
                                               (format nil "relative ~(~a~)" type)))))))
      (loop for test in *integer-float-tests*
            for type = (getf test :type)
            for limit = (getf test :limit)
            do (setf results (bench results limit type))
               (plot (format nil "integer-float ~(~a~)" type)
                     results type)
               (terpri))
      (write-results name `(quaviver:integer-float ,base) results)
      (loop with mins = (loop for test in *integer-float-tests*
                              for type = (getf test :type)
                              collect type
                              collect (loop for result in results
                                            for value = (getf result type)
                                            when value
                                              minimize value))
            for result in results
            do (ascii-table:add-row table (list* (getf result :label)
                                                 (loop for test in *integer-float-tests*
                                                       for type = (getf test :type)
                                                       for value = (getf result type)
                                                       when value
                                                         collect (format nil "~21,15g"
                                                                         (getf result type))
                                                         and collect (format nil "~21,15f"
                                                                             (/ (getf result type)
                                                                                (getf mins type)))
                                                       else
                                                         collect (make-string 21
                                                                              :initial-element #\space)
                                                         and collect (make-string 21
                                                                                  :initial-element #\space)))))
      (ascii-table:display table))))
