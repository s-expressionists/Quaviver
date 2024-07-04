;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT

(defpackage #:quaviver/math
  (:use #:common-lisp)
  (:export #:arithmetic-word
           #:expt10/32
           #:expt10/64
           #:expt10/128
           #:round-to-odd/32-64
           #:round-to-odd/64-128
           #:round-to-odd/128-256
           #:floor-multiply/32-64q64
           #:floor-multiply/evenp/32-64q64
           #:floor-multiply/64-128q128
           #:floor-multiply/evenp/64-128q128
           #:floor-log-expt
           #:ceiling-log-expt))
