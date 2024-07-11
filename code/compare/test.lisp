(cl:in-package #:quaviver/compare)

(defgeneric iterator-value-pass-p (test iterator stream))

(defgeneric test-name (test))

(defun test (tests intervals
             &key (worker-count (or (cpu-count)
                                    (error "WORKER-COUNT is required.") ))
                  (job-count 1024)
                  exit output
             &allow-other-keys)
  (setf intervals (split-interval intervals (floor job-count (length tests)))
        job-count (* (length intervals)
                     (length tests)))
  (format t "Starting ~s job~p on ~s worker~p.~%"
          job-count job-count worker-count worker-count)
  (finish-output)
  (let* ((lparallel:*kernel* (lparallel:make-kernel worker-count))
         (channel (lparallel:make-channel))
         (start-time (get-internal-real-time)))
    (loop for test in tests
          for test-pos from 0 by (length intervals)
          do (loop for interval in intervals
                   for interval-pos from 0
                   do (let ((pos (+ test-pos interval-pos))
                            (test test)
                            (interval interval))
                        (lparallel:submit-task
                         channel
                         (lambda (&aux (name (test-name test)))
                           (flet ((doit (pathname stream)
                                    (when name
                                      (write name :stream stream)
                                      (terpri stream)
                                      (finish-output stream))
                                    (handler-case (loop with iterator = (make-iterator interval)
                                                        finally (return (list pos pathname name total failures))
                                                        while (iterator-next-p iterator)
                                                        count t into total
                                                        count (not (iterator-value-pass-p test
                                                                                          iterator
                                                                                          stream))
                                                          into failures)
                                      (error (condition)
                                        (declare (ignore condition))
                                        (list pos pathname name 0 0)))))
                             (if output
                                 (uiop:with-temporary-file (:stream stream :pathname pathname :keep t)
                                   (doit pathname stream))
                                 (doit nil (make-broadcast-stream)))))))))
    (loop with counts = (make-hash-table :test #'equalp)
          with pathnames = (make-list job-count)
          for job from 1 upto job-count
          for (pos pathname name job-total job-failures) = (lparallel:receive-result channel)
          for count = (gethash name counts)
          finally (when output
                    (uiop:concatenate-files (remove nil pathnames) output)
                    (mapc #'uiop:delete-file-if-exists (remove nil pathnames)))
                  (terpri)
                  (loop for (total . failures) being each hash-value in counts using (hash-key name)
                        finally (when exit
                                  (uiop:quit (if (zerop total-failures) 0 1)))
                        do (format t "Completed ~@[~a ~]in ~/quaviver-compare:internal-time/ with ~d failure~p out of ~d tests~%"
                                   name
                                   (- (get-internal-real-time) start-time)
                                   failures failures total)
                        sum failures into total-failures)
          when count
            do (incf (car count) job-total)
               (incf (cdr count) job-failures)
          else
            do (setf (gethash name counts) (cons job-total job-failures))
          do (setf (car (nthcdr pos pathnames)) pathname)
             (format t "Completed ~@[~a ~]~a of ~a with ~a failure~p out of ~a tests.~%"
                     name job job-count
                     job-failures job-failures job-total)
             (finish-output))))
