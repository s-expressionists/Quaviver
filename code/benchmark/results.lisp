(in-package #:quaviver/benchmark)

(defvar *database-path* (make-pathname :directory '(:relative "benchmark")))

(defun write-results (name key value)
  (ensure-directories-exist *database-path*)
  (let* ((path (merge-pathnames (concatenate 'string name ".sexp")
                                *database-path*))
         (current (when (probe-file path)
                    (with-open-file (stream path)
                      (with-standard-io-syntax
                        (read stream)))))
         (pair (assoc key current :test #'equalp)))
    (if pair
        (setf (cdr pair) value)
        (setf current (acons key value current)))
    (with-open-file (stream path
                     :if-exists :supersede :if-does-not-exist :create
                     :direction :output)
        (with-standard-io-syntax
          (write current :stream stream))))
  (values))

(defun bench (func tests clients)
  (loop with results = (copy-tree clients)
        for test in tests
        for type = (getf test :type)
        finally (return results)
        do (loop for client in results
                 for client-instance = (apply #'make-instance
                                              (getf client :initargs))
                 when (member type (getf client :types))
                   do (apply func client-instance test)
                      (setf (cdr (last client))
                            (list type
                                  (the-cost-of-nothing:benchmark (apply func client-instance test)))))))
