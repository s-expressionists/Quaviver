(in-package #:quaviver/ansi-test)

(defvar *tests*
  '("FORMAT.F."
    "FORMAT.E."
    "PRINT.SHORT-FLOAT."
    "PRINT.SINGLE-FLOAT."
    "PRINT.DOUBLE-FLOAT."
    "PRINT.LONG-FLOAT."))

(defvar *extrinsic-symbols*
  '(incless-extrinsic:pprint
    incless-extrinsic:prin1
    incless-extrinsic:prin1-to-string
    incless-extrinsic:princ
    incless-extrinsic:princ-to-string
    incless-extrinsic:print
    incless-extrinsic:print-object
    incless-extrinsic:print-unreadable-object
    incless-extrinsic:write
    incless-extrinsic:write-to-string
    inravina-extrinsic:*print-pprint-dispatch*
    inravina-extrinsic:copy-pprint-dispatch
    inravina-extrinsic:pprint-dispatch
    inravina-extrinsic:pprint-exit-if-list-exhausted
    inravina-extrinsic:pprint-fill
    inravina-extrinsic:pprint-indent
    inravina-extrinsic:pprint-linear
    inravina-extrinsic:pprint-logical-block
    inravina-extrinsic:pprint-newline
    inravina-extrinsic:pprint-pop
    inravina-extrinsic:pprint-tab
    inravina-extrinsic:pprint-tabular
    inravina-extrinsic:set-pprint-dispatch
    inravina-extrinsic:with-standard-io-syntax
    invistra-extrinsic:format
    invistra-extrinsic:formatter))

(defclass burger-dybvig-1
    (inravina-extrinsic:extrinsic-client
     quaviver/burger-dybvig:client-1)
  ())

(defclass burger-dybvig-2
    (inravina-extrinsic:extrinsic-client
     quaviver/burger-dybvig:client-2)
  ())

(defvar *client-initargs*
  '(#+(or)(burger-dybvig-1)
    (burger-dybvig-2)))

(defun test (&rest args &key exit &allow-other-keys)
  (setf args (remprop :exit args))
  (loop with system = (asdf:find-system :quaviver/ansi-test)
        with ansi-directory = (merge-pathnames (make-pathname
                                                :directory '(:relative
                                                             "dependencies"
                                                             "ansi-test"))
                                               (asdf:component-pathname system))
        with expected-failures = (asdf:component-pathname
                                  (asdf:find-component system
                                                       '("code" "expected-failures.sexp")))
        for client-initargs in *client-initargs*
        for incless-extrinsic:*client* = (apply #'make-instance client-initargs)
        finally (format t "~a of ~a algorithms passed.~%" success-count (length *client-initargs*))
                (if exit
                    (uiop:quit (if (= success-count (length *client-initargs*))
                                   0
                                   1))
                    (return (= success-count (length *client-initargs*))))
        do (format t "~@<Testing ~s~:i~{ ~s ~s~^~:_~}~:>~%" (car client-initargs) (cdr client-initargs))
        count (apply #'ansi-test-harness:ansi-test
                     :directory ansi-directory
                     :expected-failures expected-failures
                     :extrinsic-symbols *extrinsic-symbols*
                     :tests *tests*
                     args) into success-count))
