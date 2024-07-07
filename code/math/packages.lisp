;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT

(defpackage #:quaviver/math
  (:use #:common-lisp)
  (:shadow #:expt)
  (:export #:arithmetic-word
           #:hi/64
           #:hi/hi64/128
           #:expt/32-10
           #:expt/64-10
           #:expt/128-10
           #:expt/256-10
           #:expt
           #:round-to-odd/32
           #:round-to-odd/64
           #:round-to-odd/128
           #:round-to-odd/256
           #:round-to-odd
           #:floor-multiply/32-64q64
           #:floor-multiply/evenp/32-64q64
           #:floor-multiply/64-128q128
           #:floor-multiply/evenp/64-128q128
           #:floor-log-expt
           #:ceiling-log-expt))

#+sbcl
(pushnew :quaviver/math/smallnum *features*)
