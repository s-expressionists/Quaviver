;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT

(cl:in-package #:quaviver/compare)

(defun distribute-range (lower upper-exclusive parts)
  (assert (<= 1 parts (- upper-exclusive lower)))
  (multiple-value-bind (increment remainder) (floor (- upper-exclusive lower) parts)
    (loop for a = lower then b
          for b = (+ a increment (cond ((plusp remainder) (decf remainder) 1)
                                       (t 0)))
          repeat parts
          collect (cons a b))))

(defun integer-string-zero-padded (integer limit-inclusive)
  (let ((n-digits (length (princ-to-string limit-inclusive))))
    (format nil "~v,,,'0@A" n-digits integer)))

(defvar *ieee754-client* (make-instance 'quaviver/ieee754:client))

(defvar *clients*
 '(:burger-dybvig (quaviver/burger-dybvig:client)
   :schubfach (quaviver/schubfach:client)))

;;; Assumes IEEE-754 binary32 and binary64 for SINGLE-FLOAT and
;;; DOUBLE-FLOAT, respectively.
(defun float-integer (clients float-type base bits)
  (when (ecase float-type               ; skip infinities and NaNs
          (single-float (eql (ldb (byte 8 0) -1)
                             (ldb (byte 8 23) bits)))
          (double-float (eql (ldb (byte 11 0) -1)
                             (ldb (byte 11 52) bits))))
    (return-from float-integer nil))
  (loop with value = (quaviver:bits-float *ieee754-client* float-type bits)
        with result
        for (name initargs) on clients by #'cddr
        for intrep = (handler-case (multiple-value-list
                                    (quaviver:float-integer
                                     (apply #'make-instance initargs) base value))
                       (error (e) e))
        do (cond
             ((typep intrep 'error)
              (setf result (list* intrep (list name) result)))
             (t
              (loop with matched-p = nil
                    for tail = result then (cddr tail)
                    until (endp tail)
                    do (let ((other-intrep (car tail))
                             (other-names (cadr tail)))
                         (when (equalp intrep other-intrep)
                           (setf matched-p t
                                 (cadr tail) (cons name other-names))))
                    finally (unless matched-p
                              (setf result (list* intrep (list name) result))))))
        finally (return (if (null (cddr result)) nil ; all matched
                            (list* bits value (nreverse result))))))

;;; Tests only positive floats.
(defun float-integer-random (clients float-type base count)
  (loop with limit = (ecase float-type
                       (single-float (ash 1 32))
                       (double-float (ash 1 64)))
        repeat count
        for bits = (random limit)
        do (float-integer clients float-type base bits)))

(defun float-integer-range
    (clients float-type base lower upper-exclusive output &optional (parts 1))
  (etypecase output
    (stream
     (loop with lparallel:*kernel* = (lparallel:make-kernel parts :name "quaviver/compare")
           with channel = (lparallel:make-channel)
           with ranges = (distribute-range lower upper-exclusive parts)
           for (sublower . subupper-exclusive) in ranges
           for file-num from 1
           for prefix = (format nil "quaviver-compare-~A-"
                                (integer-string-zero-padded file-num parts))
           do (let ((sublower sublower)
                    (subupper-exclusive subupper-exclusive)
                    (prefix prefix))
                (lparallel:submit-task
                 channel
                 (lambda ()
                   (let ((*print-pretty* nil))
                     (uiop:with-temporary-file (:stream stream :pathname pathname
                                                :prefix prefix :keep t)
                       (loop for bits from sublower below subupper-exclusive
                             for discrepancy = (float-integer clients float-type base bits)
                             do (unless (null discrepancy)
                                  (format stream "~S~%" discrepancy)))
                       :close-stream
                       pathname)))))
           finally (let ((pathnames (loop repeat parts
                                          collect (lparallel:receive-result channel))))
                     (lparallel:end-kernel :wait t)
                     (setf pathnames (sort pathnames #'string< :key #'uiop:unix-namestring))
                     (uiop:concatenate-files pathnames output)
                     (loop for pathname in pathnames
                           do (uiop:delete-file-if-exists pathname)))))
    (pathname
     (uiop:with-output-file (output output :if-exists :supersede)
       (float-integer-range clients float-type base lower upper-exclusive output parts)))))
