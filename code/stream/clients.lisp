(in-package #:quaviver/stream)

(defclass blub-impl (quaviver/blub:client
                     quaviver/dragonbox:nearest-client
                     quaviver/liebler:client)
  ())

(defclass c-impl (quaviver/c:client
                  quaviver/dragonbox:nearest-client
                  quaviver/liebler:client)
  ())

(defclass common-lisp-impl (quaviver/common-lisp:client
                            quaviver/dragonbox:nearest-client
                            quaviver/liebler:client)
  ())

(defclass fortran-impl (quaviver/fortran:client
                        quaviver/dragonbox:nearest-client
                        quaviver/liebler:client)
  ())

(defclass json-impl (quaviver/json:client
                     quaviver/dragonbox:nearest-client
                     quaviver/liebler:client)
  ())

(defclass python-impl (quaviver/python:client
                       quaviver/dragonbox:nearest-client
                       quaviver/liebler:client)
  ())

(defvar *clients*
  (list :blub (make-instance 'blub-impl)
        :c89 (make-instance 'c-impl :standard :c89)
        :c99 (make-instance 'c-impl :standard :c99)
        :c11 (make-instance 'c-impl :standard :c11)
        :c17 (make-instance 'c-impl :standard :c17)
        :c23 (make-instance 'c-impl :standard :c23)
        :c++98 (make-instance 'c-impl :standard :c++98)
        :c++03 (make-instance 'c-impl :standard :c++03)
        :c++11 (make-instance 'c-impl :standard :c++11)
        :c++14 (make-instance 'c-impl :standard :c++14)
        :c++17 (make-instance 'c-impl :standard :c++17)
        :c++20 (make-instance 'c-impl :standard :c++20)
        :c++23 (make-instance 'c-impl :standard :c++23)
        :c++26 (make-instance 'c-impl :standard :c++26)
        :common-lisp (make-instance 'common-lisp-impl
                                    :extended-exponent-sign nil)
        :common-lisp/ccl (make-instance 'common-lisp-impl
                                        :extended-exponent-sign t)
        :fortran (make-instance 'fortran-impl)
        :json (make-instance 'json-impl)
        :python (make-instance 'python-impl)))
