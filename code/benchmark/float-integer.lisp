(cl:in-package #:quaviver/benchmark)

(defvar *clients*
  `((:label "Burger-Dybvig" :initargs (quaviver/burger-dybvig:client))
    (:label "Schubfach"       :initargs (quaviver/schubfach:client))
    #+(or abcl ccl clasp cmucl ecl sbcl)
    (:label ,(format nil
                     "Native (~a)"
                     (lisp-implementation-type))
                              :initargs (quaviver/native:benchmark-client))))

(defun float-integer (&optional (base 10))
  (labels ((bench (clients limit key)
             (mapcar (lambda (properties
                              &aux (client (apply #'make-instance
                                                  (getf properties :initargs))))
                       ;; Do one conversion in case there is some initialization needed.
                       (quaviver:float-integer client base (random limit))
                       (list* key
                              (the-cost-of-nothing:benchmark
                               (quaviver:float-integer client
                                                       base
                                                       (* (1- (ash (random 2) 1))
                                                          (random limit))))
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
    (let ((results *clients*)
          (table (ascii-table:make-table '("client"
                                           "         absolute single-float"
                                           "relative single-float"
                                           "         absolute double-float"
                                           "relative double-float"
                                           #+(and ecl long-float)
                                           "           absolute long-float"
                                           #+(and ecl long-float)
                                           "  relative long-float"))))
      (setf results (bench results
                           most-positive-single-float
                           :single-time))
      (setf results (bench results
                           most-positive-double-float
                           :double-time))
      #+(and ecl long-float)
      (setf results (bench results
                           most-positive-long-float
                           :long-time))
      (plot "float-integer single-float" results :single-time)
      (terpri)
      (plot "float-integer double-float" results :double-time)
      (terpri)
      #+(and ecl long-float) (plot "float-integer long-float" results :long-time)
      #+(and ecl long-float) (terpri)
      (loop with min-single = (loop for result in results
                                    minimize (getf result :single-time))
            with min-double = (loop for result in results
                                    minimize (getf result :double-time))
            with min-long = (loop for result in results
                                    minimize (getf result :long-time 0))
            for result in results
            do (ascii-table:add-row table (list (getf result :label)
                                                (format nil "~30g" (getf result :single-time))
                                                (format nil "~21,15f" (/ (getf result :single-time)
                                                                  min-single))
                                                (format nil "~30g" (getf result :double-time))
                                                (format nil "~21,15f" (/ (getf result :double-time)
                                                                  min-double))
                                                #+(and ecl long-float)
                                                (format nil "~30g" (getf result :long-time))
                                                #+(and ecl long-float)
                                                (format nil "~21,15f" (/ (getf result :long-time)
                                                                  min-long)))))
      (ascii-table:display table))))
