(cl:in-package #:quaviver/benchmark)

(defvar *clients*
  '((:label "Burger-Dybvig 1" :initargs (quaviver/burger-dybvig:client-1))
    (:label "Burger-Dybvig 2" :initargs (quaviver/burger-dybvig:client-2))
    (:label "Schubfach"       :initargs (quaviver/schubfach:client))))

(defun float-decimal ()
  (labels ((bench (clients limit key)
             (mapcar (lambda (properties
                              &aux (client (apply #'make-instance
                                                  (getf properties :initargs))))
                       (list* key
                              (the-cost-of-nothing:benchmark (quaviver:float-decimal client
                                                                                     (random limit)))
                              properties))
                     clients))
           (plot (title results key)
             (write-string (cl-spark:vspark
                            (mapcar (lambda (properties)
                                      (getf properties key))
                                    results)
                            :title title
                            :min 0
                            :size 132
                            :labels (mapcar (lambda (client)
                                              (getf client :label))
                                            results)))))
    (let ((results (bench (bench *clients*
                                 most-positive-single-float
                                 :single-time)
                          most-positive-double-float
                          :double-time))
          (table (ascii-table:make-table '("client"
                                           "         absolute single-float"
                                           "relative single-float"
                                           "         absolute double-float"
                                           "relative double-float"))))
      (plot "float-decimal single-float" results :single-time)
      (terpri)
      (plot "float-decimal double-float" results :double-time)
      (terpri)
      (loop with min-single = (loop for result in results
                                    minimize (getf result :single-time))
            with min-double = (loop for result in results
                                    minimize (getf result :double-time))
            for result in results
            do (ascii-table:add-row table (list (getf result :label)
                                                (format nil "~30g" (getf result :single-time))
                                                (format nil "~21,15f" (/ (getf result :single-time)
                                                                  min-single))
                                                (format nil "~30g" (getf result :double-time))
                                                (format nil "~21,15f" (/ (getf result :double-time)
                                                                  min-double)))))
      (ascii-table:display table))))
