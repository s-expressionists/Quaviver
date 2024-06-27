;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT

(cl:defpackage #:quaviver/compare
  (:use #:common-lisp)
  (:nicknames #:quaviver-compare)
  (:export #:*clients*
           #:bit-interval
           #:bit-part-interval
           #:test
           #:float-integer
           #:float-integer/bd.s/f
           #:float-integer/bd.s/d
           #:float-integer/s.d/f
           #:float-integer/s.d/d
           #:integer-float
           #:integer-float/j.l/f
           #:integer-float/j.l/d))
