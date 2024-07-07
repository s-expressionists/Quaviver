;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT

(defpackage #:quaviver/math
  (:use #:common-lisp)
  (:shadow #:expt)
  (:export #:arithmetic-word
           #:hi/64
           #:hi/hi64/128
           #:expt
           #:round-to-odd
           #:floor-multiply/32-64q64
           #:floor-multiply/evenp/32-64q64
           #:floor-multiply/64-128q128
           #:floor-multiply/evenp/64-128q128
           #:floor-log-expt
           #:ceiling-log-expt))

#+sbcl
(pushnew :quaviver/math/smallnum *features*)
