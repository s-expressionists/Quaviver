(in-package #:quaviver)

(deftype uint-32 ()
  '(integer 0 #xFFFFFFFF))

(deftype uint-64 ()
  '(integer 0 #xFFFFFFFFFFFFFFFF))

(deftype uint-128 ()
  '(integer 0 #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF))

(declaim (ftype (function (uint-64 uint-32) uint-32)
                round-to-odd/32)
         (ftype (function (uint-128 uint-64) uint-64)
                round-to-odd/64)
         (ftype (function (fixnum) uint-64)
                integer-expt10/32)
         (ftype (function (fixnum) uint-128)
                integer-expt10/64)
         (ftype (function (fixnum) fixnum)
                floor-log2-expt10
                floor-log10-expt2)
         (inline round-to-odd/32
                 round-to-odd/64
                 integer-expt10/32
                 integer-expt10/64
                 floor-log2-expt10
                 floor-log10-expt2))

(defun round-to-odd/32 (g cp)
  (let ((p (* g cp)))
    (logior (ldb (byte 32 64) p)
            (if (> (ldb (byte 32 32) p) 1) 1 0))))

(defun round-to-odd/64 (g cp)
  (let ((p (* g cp)))
    (logior (ldb (byte 64 128) p)
            (if (> (ldb (byte 64 64) p) 1) 1 0))))

(defun compute-expt10 (k-min k-max width &optional (base 10))
  (make-array (- k-max k-min -1)
              :element-type `(integer 0 ,(1- (ash 1 width)))
              :initial-contents (loop for k from k-min to k-max
                                      for l = (expt base k)
                                      collect (ldb (byte width 0)
                                                   (ceiling (* (expt 2 (- width (floor (log l 2)) 1))
                                                               l))))))

(defvar *expt10/values/32* nil)

(defconstant +expt10/min-exponent/32 -31)

(defconstant +expt10/max-exponent/32 52)

(defun expt10/32 (power)
  (aref (or *expt10/values/32*
             (setf *expt10/values/32* (compute-expt10 +expt10/min-exponent/32
                                                      +expt10/max-exponent/32
                                                      64)))
         (- (- +expt10/min-exponent/32) power)))

(defvar *expt10/values/64* nil)

(defconstant +expt10/min-exponent/64 -292)

(defconstant +expt10/max-exponent/64 340)

(defun expt10/64 (power)
  (aref (or *expt10/values/64*
             (setf *expt10/values/64* (compute-expt10 +expt10/min-exponent/64
                                                      +expt10/max-exponent/64
                                                      128)))
         (- (- +expt10/min-exponent/64) power)))

(defun floor-log2-expt10 (e)
  (ash (* e 1741647) -19))

(defun floor-log10-expt2 (e &optional three-quarters-p)
  (ash (- (* e 1262611)
          (if three-quarters-p 524031 0))
       -22))
