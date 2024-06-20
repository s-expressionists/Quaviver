(cl:in-package #:quaviver/benchmark)

(defvar *tests*
  (list `(:type single-float :limit ,most-positive-single-float)
        `(:type double-float :limit ,most-positive-double-float)
        #+quaviver/long-float
        `(:type long-float   :limit ,most-positive-long-float)))

(defvar *clients*
  `((:label "Burger-Dybvig"
     :initargs (quaviver/burger-dybvig:client)
     :types (single-float double-float long-float))
    (:label "Schubfach"
     :initargs (quaviver/schubfach:client)
     :types (single-float double-float long-float))
    #+(or abcl ccl clasp cmucl ecl sbcl)
    (:label "Native"
     :initargs (quaviver/native:benchmark-client)
     :types (single-float double-float long-float))))

(defun float-integer (&key (base 10)
                           (name (uiop:implementation-identifier)))
  (labels ((bench (clients limit key)
             (mapcar (lambda (properties
                              &aux (client (apply #'make-instance
                                                  (getf properties :initargs))))
                       (cond ((member key (getf properties :types))
                              ;; Do one conversion in case there is some initialization needed.
                              (quaviver:float-integer client base (random limit))
                              (list* key
                                     (the-cost-of-nothing:benchmark
                                      (quaviver:float-integer client
                                                              base
                                                              (* (1- (ash (random 2) 1))
                                                                 (random limit))))
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
    (let ((results *clients*)
          (table (ascii-table:make-table
                  (list* "client"
                         (loop for test in *tests*
                               for type = (getf test :type)
                               collect (format nil "~21@a"
                                               (format nil "absolute ~(~a~)" type))
                               collect (format nil "~21@a"
                                               (format nil "relative ~(~a~)" type)))))))
      (loop for test in *tests*
            for type = (getf test :type)
            for limit = (getf test :limit)
            do (setf results (bench results limit type))
               (plot (format nil "float-integer ~(~a~)" type)
                     results type)
               (terpri))
      (write-results name `(quaviver:float-integer ,base) results)
      (loop with mins = (loop for test in *tests*
                              for type = (getf test :type)
                              collect type
                              collect (loop for result in results
                                            for value = (getf result type)
                                            when value
                                              minimize value))
            for result in results
            do (ascii-table:add-row table (list* (getf result :label)
                                                 (loop for test in *tests*
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
