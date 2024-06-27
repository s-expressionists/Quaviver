;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT

(cl:defpackage #:quaviver/compare
  (:use #:common-lisp)
  (:nicknames #:quaviver-compare)
  (:export #:*float-integer-clients*
           #:float-integer/compare
           #:float-integer/compare-bits
           #:float-integer/random
           #:float-integer/range
           #:float-integer/range/parallel
           #:*integer-float-clients*
           #:integer-float/compare
           #:integer-float/compare-bits
           #:integer-float/random
           #:integer-float/range
           #:integer-float/range/parallel))
