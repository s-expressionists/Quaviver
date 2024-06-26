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

(defmacro %round-to-odd-1 (g cp size)
  `(let ((p (* ,g ,cp)))
     (logior (ldb (byte ,size ,(ash size 1)) p)
             (if (> (ldb (byte ,size ,size) p) 1) 1 0))))

(defmacro %round-to-odd-2 (g cp size)
  `(let ((p (ash (* ,g ,cp) ,(- size))))
     (if (ldb-test (byte ,(1- size) 1) p)
         (logior (ash p ,(- size)) 1)
         (ash p ,(- size)))))

(defun round-to-odd/32 (g cp)
  #-(or ecl cmucl) (%round-to-odd-1 g cp 32)
  #+(or ecl cmucl) (%round-to-odd-2 g cp 32))

(defun round-to-odd/64 (g cp)
  (%round-to-odd-2 g cp 64))

(defun round-to-odd/128 (g cp)
  (%round-to-odd-2 g cp 128))

(defconstant +expt10/min-exponent/32+ -53)

(defconstant +expt10/max-exponent/32+ 53)

(defvar *expt10/values/32*
  (compute-expt10 +expt10/min-exponent/32+ +expt10/max-exponent/32+ 64))

(defun expt10/32 (power)
  (svref *expt10/values/32*
         (- (- +expt10/min-exponent/32+) power)))

(defconstant +expt10/min-exponent/64+ -342)

(defconstant +expt10/max-exponent/64+ 342)

(defvar *expt10/values/64*
  (compute-expt10 +expt10/min-exponent/64+ +expt10/max-exponent/64+ 128))

(defun expt10/64 (power)
  (svref *expt10/values/64*
         (- (- +expt10/min-exponent/64+) power)))

(defconstant +expt10/min-exponent/128+ -5023)

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

(defconstant +min-base+ 2)

(defconstant +max-base+ 36)

(defconstant +log-expt-shift+ 22)

(defvar *log-expt*
  (compute-log-expt +min-base+ +max-base+ +log-expt-shift+))

(defvar *log-3/4*
  (compute-log-expt +min-base+ +max-base+ +log-expt-shift+))

(defun floor-log-expt (log-base expt-base exp &optional three-quarters-p)
  (ash (+ (* exp (aref *log-expt*
                       (- log-base +min-base+)
                       (- expt-base +min-base+)))
          (if three-quarters-p
              (svref *log-3/4* (- log-base +min-base+))
              0))
       (- +log-expt-shift+)))

(defun ceiling-log-expt (log-base expt-base exp &optional three-quarters-p)
  (values (ceiling (+ (* exp (aref *log-expt*
                                   (- log-base +min-base+)
                                   (- expt-base +min-base+)))
                      (if three-quarters-p
                          (svref *log-3/4* (- log-base +min-base+))
                          0))
                   (ash 1 +log-expt-shift+))))
