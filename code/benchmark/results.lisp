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
