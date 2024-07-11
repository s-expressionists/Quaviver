;;;; SPDX-FileCopyrightText: Copyright (c) 2024 s-expressionists
;;;; SPDX-License-Identifier: MIT

(in-package #:quaviver.math)

(declaim (ftype (function ((arithmetic-word 32) (arithmetic-word 32 2))
                          (arithmetic-word 32))
                round-to-odd/32)
         (ftype (function ((arithmetic-word 64) (arithmetic-word 64 2))
                          (arithmetic-word 64))
                round-to-odd/64)
         (ftype (function ((arithmetic-word 128) (arithmetic-word 128 2))
                          (arithmetic-word 128))
                round-to-odd/128)
         (ftype (function ((arithmetic-word 256) (arithmetic-word 256 2))
                          (arithmetic-word 256))
                round-to-odd/256)
         (inline round-to-odd/32
                 round-to-odd/64
                 round-to-odd/128
                 round-to-odd/256))

(defmacro %round-to-odd-1 (size cp g)
  `(let ((p (* ,cp ,g)))
     (logior (ldb (byte ,size ,(ash size 1)) p)
             (if (> (ldb (byte ,size ,size) p) 1) 1 0))))

(defmacro %round-to-odd-2 (size cp g)
  `(let ((p (ash (* ,cp ,g) ,(- size))))
     (if (ldb-test (byte ,(1- size) 1) p)
         (logior (ash p ,(- size)) 1)
         (ash p ,(- size)))))

(defun round-to-odd/32 (cp g)
  #+quaviver.math/smallnum
  (let ((p (*/32-64/hi64 cp g)))
    (if (ldb-test (byte 31 1) p)
        (logior (ash p -32) 1)
        (ash p -32)))
  #+(and (not quaviver.math/smallnum) (not (or ecl cmucl)))
  (%round-to-odd-1 32 cp g)
  #+(and (not quaviver.math/smallnum) (or ecl cmucl))
  (%round-to-odd-2 32 cp g))

(defun round-to-odd/64 (cp g)
  #+quaviver.math/smallnum
  (multiple-value-bind (ph pl)
      (*/64-128/hi128 cp (aref g 0) (aref g 1))
    (if (ldb-test (byte 63 1) pl)
        (logior ph 1)
        ph))
  #-quaviver.math/smallnum
  (%round-to-odd-2 64 cp g))

(defun round-to-odd/128 (cp g)
  (%round-to-odd-2 128 cp g))

(defun round-to-odd/256 (cp g)
  (%round-to-odd-2 256 cp g))

(defun round-to-odd (arithmetic-size cp g)
  (ecase arithmetic-size
    (32
     (round-to-odd/32 cp g))
    (64
     (round-to-odd/64 cp g))
    (128
     (round-to-odd/128 cp g))
    (256
     (round-to-odd/256 cp g))))

(define-compiler-macro round-to-odd (&whole whole arithmetic-size cp g)
  (case arithmetic-size
    (32
     `(round-to-odd/32 ,cp ,g))
    (64
     `(round-to-odd/64 ,cp ,g))
    (128
     `(round-to-odd/128 ,cp ,g))
    (256
     `(round-to-odd/256 ,cp ,g))
    (otherwise
     whole)))
