;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT

(cl:defpackage #:quaviver/compare
  (:use #:common-lisp)
  (:nicknames #:quaviver-compare)
  (:export #:*clients*
           #:bit-interval
           #:bit-part-interval
           #:test
           #:float-triple
           #:float-triple/bd.s/f
           #:float-triple/bd.s/d
           #:float-triple/s.d/f
           #:float-triple/s.d/d
           #:triple-float
           #:triple-float/j.l/f
           #:triple-float/j.l/d))
