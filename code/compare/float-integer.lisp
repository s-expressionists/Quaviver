;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT

(cl:in-package #:quaviver/compare)

(defun distribute-range (start end parts)
  (assert (<= 1 parts (- end start -1)))
  (multiple-value-bind (increment remainder) (floor (- end start -1) parts)
    (loop for a = start then b
          for b = (+ a increment (cond ((plusp remainder) (decf remainder) 1)
                                       (t 0)))
          repeat parts
          collect (cons a (1- b)))))

(defvar *ieee754-client* (make-instance 'quaviver/ieee754:client))

(defvar *clients*
 '(:burger-dybvig (quaviver/burger-dybvig:client)
   :schubfach (quaviver/schubfach:client)))

(defun float-integer/compare (client-instance-1 client-instance-2 base value)
  (multiple-value-bind (significand1 exponent1 sign1)
      (ignore-errors (quaviver:float-integer client-instance-1 base value))
    (multiple-value-bind (significand2 exponent2 sign2)
        (ignore-errors (quaviver:float-integer client-instance-2 base value))
      (unless (or (and (null significand1) ; both signaled errors
                       (null significand2))
                  (and (eql significand1 significand2) ; identical results
                       (eql exponent1 exponent2)
                       (eql sign1 sign2))
                  (and (eql sign1 sign2) ; significands have trailing zeros
                       (integerp significand2)
                       (integerp significand2)
                       (integerp exponent1)
                       (integerp exponent2)
                       (eql (* significand1
                               (expt base
                                     (- exponent1
                                        (min exponent1 exponent2))))
                            (* significand2
                               (expt base
                                     (- exponent2
                                        (min exponent1 exponent2)))))))
        (list value
              (when significand1
                (list significand1 exponent1 sign1))
              (when significand2
                (list significand2 exponent2 sign2)))))))

(defun float-integer/compare-bits (client-instance-1 client-instance-2 float-type base bits)
  (let ((result (float-integer/compare client-instance-1 client-instance-2
                                       base
                                       (quaviver:bits-float *ieee754-client* float-type bits))))
    (when result
      (list* bits result))))

(defun float-size (float-type)
  (ecase float-type
    (short-float 32)
    (single-float 32)
    (double-float 64)
    (long-float
     #-quaviver/long-float 64
     #+quaviver/long-float 80)))

(defun float-hex-digits (float-type)
  (/ (float-size float-type) 4))

(defun write-discrepancy (float-type discrepancy &optional (stream t))
  (format stream "~:<#x~v,'0x ~e ~s ~s~:@>~%"
          (list* (float-hex-digits float-type) discrepancy)))

(defun float-integer/random (name1 name2 float-type base count &optional (stream t))
  (loop with limit = (ash 1 (float-size float-type))
        with client-instance-1 = (apply #'make-instance (getf *clients* name1))
        with client-instance-2 = (apply #'make-instance (getf *clients* name2))
        with *print-base* = base
        with pass = t
        repeat count
        for bits = (random limit)
        for discrepancy = (float-integer/compare-bits client-instance-1 client-instance-2
                                                      float-type base bits)
        finally (return pass)
        when discrepancy
          do (setf pass nil)
             (write-discrepancy float-type discrepancy stream)))

(defun float-integer/range (name1 name2 float-type base start end &optional (stream t))
  (loop with client-instance-1 = (apply #'make-instance (getf *clients* name1))
        with client-instance-2 = (apply #'make-instance (getf *clients* name2))
        with *print-base* = base
        with pass = t
        for bits from start upto end
        for discrepancy = (float-integer/compare-bits client-instance-1 client-instance-2
                                                      float-type base bits)
        finally (return pass)
        when discrepancy
          do (setf pass nil)
             (write-discrepancy float-type discrepancy stream)))

(defun float-integer/range/parallel (name1 name2 output
                                     &key (float-type 'single-float) (base 10)
                                          (start 0) end
                                          (worker-count (or (cpu-count)
                                                            (error "WORKER-COUNT is required.") ))
                                          (job-count 1024))
  (setf end (or end
                (1- (ash 1 (float-size float-type))))
        job-count (min (- end start) job-count))
  (format t "Starting ~s job~p on ~s worker~p.~%"
          worker-count worker-count job-count job-count)
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
                             (float-integer/range name1 name2 float-type base
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
