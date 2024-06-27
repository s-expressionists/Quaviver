(cl:in-package #:quaviver/compare)

(defgeneric iterator-value-pass-p (test iterator stream))

(defun test (tests intervals output
             &key (worker-count (or (cpu-count)
                                    (error "WORKER-COUNT is required.") ))
                  (job-count 1024)
                  exit
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
                         (lambda ()
                           (uiop:with-temporary-file (:stream stream :pathname pathname :keep t)
                             (handler-case (loop with iterator = (make-iterator interval)
                                                 finally (return (list pos pathname total failures))
                                                 while (iterator-next-p iterator)
                                                 count t into total
                                                 count (not (iterator-value-pass-p test
                                                                                   iterator
                                                                                   stream))
                                                   into failures)
                               (error (condition)
                                 (declare (ignore condition))
                                 (list pos pathname 0 0)))))))))
    (loop with total = 0
          with failures = 0
          with pathnames = (make-list job-count)
          for job from 1 upto job-count
          for (pos pathname job-total job-failures)  = (lparallel:receive-result channel)
          finally (uiop:concatenate-files (remove nil pathnames) output)
                  (mapc #'uiop:delete-file-if-exists (remove nil pathnames))
                  (format t "Completed in ~/quaviver-compare:internal-time/ with ~d failure~p out of ~d tests~%"
                          (- (get-internal-real-time) start-time)
                          failures failures total)
                  (when exit
                    (uiop:quit (if (zerop failures) 0 1)))
          do (setf (car (nthcdr pos pathnames)) pathname)
             (incf total job-total)
             (incf failures job-failures)
             (format t "Completed ~a of ~a with ~a failure~p out of ~a tests.~%"
                     job job-count
                     job-failures job-failures job-total)
             (finish-output))))
