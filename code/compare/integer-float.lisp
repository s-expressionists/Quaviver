;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT

(cl:in-package #:quaviver/compare)

(defvar *reference-float-integer-client* (make-instance 'quaviver/schubfach:client))

(defvar *integer-float-clients*
 '(:jaffer (quaviver/jaffer:client)
   :liebler (quaviver/liebler:client)))

(defun integer-float/compare
    (client-instance-1 client-instance-2 float-type base significand exponent sign)
  (let ((value1 (ignore-errors (quaviver:integer-float client-instance-1 float-type
                                                       base significand exponent sign)))
        (value2 (ignore-errors (quaviver:integer-float client-instance-2 float-type
                                                       base significand exponent sign))))
    (unless (eql value1 value2)
      (list significand exponent sign value1 value2))))

(defun integer-float/compare-bits (client-instance-1 client-instance-2 float-type base bits)
  (multiple-value-bind (significand exponent sign)
      (quaviver:float-integer *reference-float-integer-client*
                              base
                              (quaviver:bits-float *ieee754-client* float-type bits))
    (let ((result (integer-float/compare client-instance-1 client-instance-2
                                         float-type base significand exponent sign)))
      (when result
        (list* bits result)))))

(defun write-integer-float-discrepancy (discrepancy &optional (stream t))
  (format stream "~S~%" discrepancy))

(defun integer-float/random (name1 name2 float-type base count &optional (stream t))
  (loop with limit = (ash 1 (float-size float-type))
        with client-instance-1 = (apply #'make-instance (getf *integer-float-clients* name1))
        with client-instance-2 = (apply #'make-instance (getf *integer-float-clients* name2))
        with *print-base* = base
        with pass = t
        repeat count
        for bits = (random limit)
        for discrepancy = (integer-float/compare-bits client-instance-1 client-instance-2
                                                      float-type base bits)
        finally (return pass)
        when discrepancy
          do (setf pass nil)
             (write-integer-float-discrepancy discrepancy stream)))

(defun integer-float/range (name1 name2 float-type base start end &optional (stream t))
  (loop with client-instance-1 = (apply #'make-instance (getf *integer-float-clients* name1))
        with client-instance-2 = (apply #'make-instance (getf *integer-float-clients* name2))
        with *print-base* = base
        with pass = t
        for bits from start upto end
        for discrepancy = (integer-float/compare-bits client-instance-1 client-instance-2
                                                      float-type base bits)
        finally (return pass)
        when discrepancy
          do (setf pass nil)
             (write-integer-float-discrepancy discrepancy stream)))

(defun integer-float/range/parallel (name1 name2 output
                                     &key (float-type 'single-float) (base 10)
                                          (start 0) end
                                          (worker-count (or (cpu-count)
                                                            (error "WORKER-COUNT is required.") ))
                                          (job-count 1024))
  (setf end (or end
                (1- (ash 1 (float-size float-type))))
        job-count (min (- end start) job-count))
  (format t "Starting ~s job~p on ~s worker~p.~%"
          job-count job-count worker-count worker-count)
  (finish-output)
  (let* ((lparallel:*kernel* (lparallel:make-kernel worker-count))
         (channel (lparallel:make-channel))
         (start-time (get-internal-real-time)))
    (loop for (start . end) in (distribute-range start end job-count)
          for pos from 1
          do (let ((start start)
                   (end end)
                   (pos pos))
               (lparallel:submit-task
                channel
                (lambda (&aux (discrepancy-count 0))
                  (uiop:with-temporary-file (:stream stream :pathname pathname :keep t)
                    (unless (ignore-errors
                             (integer-float/range name1 name2 float-type base
                                                  start end stream))
                      (incf discrepancy-count))
                    :close-stream
                    (list pos pathname discrepancy-count))))))
    (loop with all-discrepancy-count = 0
          with pathnames = (list* (uiop:with-temporary-file (:stream stream
                                                             :pathname pathname
                                                             :keep t)
                                    (format stream "~:@<~s ~s ~s ~s #x~v,'0x #x~v,'0x~:@>~%"
                                            name1 name2
                                            float-type base
                                            (float-hex-digits float-type) start
                                            (float-hex-digits float-type) end)
                                    (terpri stream)
                                    :close-stream
                                    pathname)
                                  (make-list job-count))
          for job from 1 upto job-count
          for (pos pathname discrepancy-count) = (lparallel:receive-result channel)
          finally (uiop:concatenate-files pathnames output)
                  (mapc #'uiop:delete-file-if-exists pathnames)
                  (format t "Completed in ~/quaviver-compare:internal-time/ with ~d discrepanc~@p.~%"
                          (- (get-internal-real-time) start-time)
                          all-discrepancy-count all-discrepancy-count)
          do (setf (car (nthcdr pos pathnames)) pathname)
             (incf all-discrepancy-count discrepancy-count)
             (format t "Completed ~a of ~a~%" job job-count)
             (finish-output))))
