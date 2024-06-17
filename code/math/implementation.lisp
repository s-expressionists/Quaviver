(in-package #:quaviver/math)

(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 32))
                          (unsigned-byte 32))
                round-to-odd/32)
         (ftype (function ((unsigned-byte 128) (unsigned-byte 64))
                          (unsigned-byte 64))
                round-to-odd/64)
         (ftype (function (fixnum) (unsigned-byte 64))
                expt10/32)
         (ftype (function (fixnum) (unsigned-byte 128))
                expt10/64)
         (ftype (function (fixnum) fixnum)
                floor-log2-expt10)
         (ftype (function (fixnum &optional boolean) fixnum)
                floor-log10-expt2)
         (inline round-to-odd/32
                 round-to-odd/64
                 expt10/32
                 expt10/64
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

(defun round-to-odd/128 (g cp)
  (let ((p (* g cp)))
    (logior (ldb (byte 128 256) p)
            (if (> (ldb (byte 128 128) p) 1) 1 0))))

(defconstant +expt10/min-exponent/32+ -31)

(defconstant +expt10/max-exponent/32+ 53)

(defvar *expt10/values/32*
  (compute-expt10 +expt10/min-exponent/32+ +expt10/max-exponent/32+ 64))

(defun expt10/32 (power)
  (svref *expt10/values/32*
         (- (- +expt10/min-exponent/32+) power)))

(defconstant +expt10/min-exponent/64+ -292)

(defconstant +expt10/max-exponent/64+ 342)

(defvar *expt10/values/64*
  (compute-expt10 +expt10/min-exponent/64+ +expt10/max-exponent/64+ 128))

(defun expt10/64 (power)
  (svref *expt10/values/64*
         (- (- +expt10/min-exponent/64+) power)))

(defconstant +expt10/min-exponent/128+ -4913)

(defconstant +expt10/max-exponent/128+ 5023)

(defvar *expt10/values/128* nil)

(defun expt10/128 (power)
  (svref (or *expt10/values/128*
             (setf *expt10/values/128*
                   (compute-expt10 +expt10/min-exponent/128+ +expt10/max-exponent/128+ 256)))
         (- (- +expt10/min-exponent/128+) power)))

(defun floor-log2-expt10 (e)
  (ash (* e 1741647) -19))

(defun floor-log10-expt2 (e &optional three-quarters-p)
  (ash (- (* e 1262611)
          (if three-quarters-p 524031 0))
       -22))
