;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT AND BSD-3-Clause
;;;;
;;;; The implementations of QUAVIVER:BITS-FLOAT and QUAVIVER:FLOAT-BITS
;;;; were ported, with modifications, from Nibbles [1], which is
;;;; licensed under the BSD-3-Clause license.
;;;; Any original code herein is licensed under the MIT license (Expat).
;;;;
;;;; [1]: https://github.com/sharplispers/nibbles

(cl:defpackage #:quaviver/ieee754
  (:use #:common-lisp)
  (:export #:client))
