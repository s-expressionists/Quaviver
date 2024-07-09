(in-package #:quaviver/benchmark)

(defvar *results* nil)

(defun report/test (name
                    &aux (implementation-width (loop for (implementation . result) in *results*
                                                     when (assoc name result :test #'equalp)
                                                       maximize (length implementation)))
                         (algo-width (loop for (implementation . result) in *results*
                                           for (nil . v) = (assoc name result :test #'equalp)
                                           when v
                                             maximize (loop for algo in v
                                                            maximize (length (getf algo :label)))))
                      (types (remove-if-not (lambda (type)
                                              (block wibble
                                                (loop for (nil . result) in *results*
                                                      for (nil . v) = (assoc name result :test #'equalp)
                                                      do (loop for algo in v
                                                               when (getf algo type)
                                                                 do (return-from wibble t)))))
                                            '(short-float single-float double-float long-float))))
  (format t "~a~%~%" name)
  (write-line
   (cl-spark:vspark
    (loop with gap = nil
          for (implementation . result) in *results*
          for (nil . v) = (assoc name result :test #'equalp)
          nconc (loop for type in types
                      for max = (loop for algo in v
                                      for val = (getf algo type)
                                      when val
                                        maximize val)
                      nconc (loop with first = t
                                  for algo in v
                                  for val = (getf algo type)
                                  when (and val first gap)
                                    collect 0
                                  when val
                                    collect (/ val max)
                                    and do (setf gap t
                                                 first nil))))
    :title "Relative Times within Implementation and Type"
    :min 0
    :size 132
    :labels (loop with gap = nil
                  for (implementation . result) in *results*
                  for (nil . v) = (assoc name result :test #'equalp)
                  nconc (loop for type in types
                              nconc (loop with first = t
                                          for algo in v
                                          for val = (getf algo type)
                                          when (and val first gap)
                                            collect ""
                                          when val
                                            do (setf gap t
                                                     first nil)
                                            and collect (format nil "~va | ~va | ~(~12a~)"
                                                            implementation-width
                                                            implementation
                                                            algo-width
                                                            (getf algo :label)
                                                            type))))))
  (format t "~%Absolute and Relative Times~%")
  (let ((table (ascii-table:make-table
                (list* "Implementation"
                       "Client"
                       (loop for type in types
                             collect (ecase type
                                       (short-float  " abs short")
                                       (single-float "abs single")
                                       (double-float "abs double")
                                       (long-float   "  abs long"))
                             collect (ecase type
                                       (short-float  " rel short")
                                       (single-float "rel single")
                                       (double-float "rel double")
                                       (long-float   "  rel long")))))))
    (loop for (implementation . result) in *results*
          for (nil . v) = (assoc name result :test #'equalp)
          for max = (loop for type in types
                           collect type
                           collect (loop for algo in v
                                         for val = (getf algo type)
                                         when val
                                           maximize val))
          do (loop for algo in v
                   do (ascii-table:add-row
                       table
                       (list* implementation
                              (getf algo :label)
                              (loop for type in types
                                    for val = (getf algo type)
                                    when val
                                      collect (format nil "~10,5g"
                                                      (coerce val 'double-float))
                                      and collect (format nil "~10,8f"
                                                          (/ (coerce val 'double-float)
                                                             (getf max type)))
                                    else
                                      collect ""
                                      and collect "")))))
    (ascii-table:display table)))


(defun report ()
  (loop with *results* = (loop for path in (directory (merge-pathnames "*.sexp" *database-path*))
                               collect (cons (pathname-name path)
                                             (with-open-file (stream path)
                                               (with-standard-io-syntax
                                                 (read stream)))))
        for test in (remove-duplicates
                     (loop for (nil . result) in *results*
                           nconc (mapcar #'car result))
                     :test #'equalp)
        do (report/test test)))

(defun report/plot-type (title results key)
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
                                 results))))

(defun report/run-summary (title tests results)
  (loop for test in tests
        for type = (getf test :type)
        do (report/plot-type (format nil "~a ~(~a~)" title type)
                             results type)
           (terpri))
  (let ((table (ascii-table:make-table
                (list* "client"
                       (loop for test in tests
                             for type = (getf test :type)
                             collect (format nil "~21@a"
                                             (format nil "absolute ~(~a~)" type))
                             collect (format nil "~21@a"
                                             (format nil "relative ~(~a~)" type)))))))
    (loop with mins = (loop for test in tests
                            for type = (getf test :type)
                            collect type
                            collect (loop for result in results
                                          for value = (getf result type)
                                          when value
                                            minimize value))
          for result in results
          do (ascii-table:add-row table
                                  (list* (getf result :label)
                                         (loop for test in tests
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
    (ascii-table:display table)))

(defun report/float-traits ()
  (let ((table (ascii-table:make-table
                (list "trait"
                      #+quaviver/short-float "short-float"
                      "single-float"
                      "double-float"
                      #+quaviver/long-float "long-float"))))
    (loop for trait in '(quaviver:storage-size
                         quaviver:significand-size
                         quaviver:exponent-size
                         quaviver:sign-size
                         quaviver:hidden-bit-p
                         quaviver:subnormalp
                         quaviver:non-number-p
                         quaviver:exponent-bias
                         quaviver:max-exponent
                         quaviver:min-exponent
                         quaviver:arithmetic-size)
          do (ascii-table:add-row table
                                  (list (string-downcase (symbol-name trait))
                                        #+quaviver/short-float (funcall trait 'short-float)
                                        (funcall trait 'single-float)
                                        (funcall trait 'double-float)
                                        #+quaviver/long-float (funcall trait 'long-float))))
    (format t "~a float traits~%" (uiop:implementation-identifier))
    (ascii-table:display table)))
