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
                (list* (format nil "~va | ~va"
                               implementation-width "Implementation"
                               algo-width "Client")
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
                       (list* (format nil "~va | ~a"
                                      (max 14 implementation-width)
                                      implementation
                                      (getf algo :label))
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
