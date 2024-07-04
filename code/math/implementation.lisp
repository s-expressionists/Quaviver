(in-package #:quaviver/math)

(deftype arithmetic-word (arithmetic-size &optional (count 1))
  #+quaviver/math/smallnum
  (ecase arithmetic-size
    (32 (case count
          (1 `(unsigned-byte 32))
          (2 `(unsigned-byte 64))
          (otherwise `(simple-array (unsigned-byte 64) (,(ceiling count 2))))))
    (64 (if (eql count 1)
            `(unsigned-byte 64)
            `(simple-array (unsigned-byte 64) (,count))))
    (128 `(unsigned-byte ,(* arithmetic-size count))))
  #-quaviver/math/smallnum
  `(unsigned-byte ,(* arithmetic-size count)))

(declaim (ftype (function ((arithmetic-word 32 2) (arithmetic-word 32))
                          (values (arithmetic-word 32) &optional))
                round-to-odd/32)
         (ftype (function ((arithmetic-word 64 2) (arithmetic-word 64))
                          (values (arithmetic-word 64) &optional))
                round-to-odd/64)
         (ftype (function ((arithmetic-word 128 2) (arithmetic-word 128))
                          (values (arithmetic-word 128) &optional))
                round-to-odd/128)
         (ftype (function (fixnum)
                          (values (arithmetic-word 32 2) &optional))
                expt10/32)
         (ftype (function (fixnum)
                          (values (arithmetic-word 64 2) &optional))
                expt10/64)
         (ftype (function (fixnum)
                          (values (arithmetic-word 128 2) &optional))
                expt10/128)
         (ftype (function (fixnum fixnum fixnum &optional boolean)
                          (values fixnum &optional))
                floor-log-expt ceiling-log-expt)
         (inline round-to-odd/32
                 round-to-odd/64
                 round-to-odd/128
                 expt10/32
                 expt10/64
                 expt10/128
                 floor-log-expt
                 ceiling-log-expt))

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
  (compute-expt +expt10/min-exponent/32+ +expt10/max-exponent/32+ 64))

(defun expt10/32 (power)
  (svref *expt10/values/32*
         (- (- +expt10/min-exponent/32+) power)))

(defconstant +expt10/min-exponent/64+ -342)

(defconstant +expt10/max-exponent/64+ 342)

(defvar *expt10/values/64*
  (compute-expt +expt10/min-exponent/64+ +expt10/max-exponent/64+ 128))

(defun expt10/64 (power)
  (svref *expt10/values/64*
         (- (- +expt10/min-exponent/64+) power)))

(defconstant +expt10/min-exponent/128+ -5023)

(defconstant +expt10/max-exponent/128+ 5023)

(defvar *expt10/values/128* nil)

(defun expt10/128 (power)
  (svref (or *expt10/values/128*
             (setf *expt10/values/128*
                   (compute-expt +expt10/min-exponent/128+ +expt10/max-exponent/128+ 256)))
         (- (- +expt10/min-exponent/128+) power)))

(defconstant +min-base+ 2)

(defconstant +max-base+ 36)

(defconstant +log-expt-shift+ 22)

(defvar *log-expt*
  (compute-log-expt +min-base+ +max-base+ +log-expt-shift+))

(defvar *log-3/4*
  (compute-log-3/4 +min-base+ +max-base+ +log-expt-shift+))

(defun floor-log-expt (log-base expt-base exp &optional three-quarters-p)
  (declare (optimize speed))
  (ash (+ (* exp (aref *log-expt*
                       (- log-base +min-base+)
                       (- expt-base +min-base+)))
          (if three-quarters-p
              (svref *log-3/4* (- log-base +min-base+))
              0))
       (- +log-expt-shift+)))

(define-compiler-macro floor-log-expt
    (&whole whole log-base expt-base exp &optional three-quarters-p)
  (if (or (not (constantp log-base))
          (not (constantp expt-base)))
      whole
      (let ((multiplier (aref *log-expt*
                              (- log-base +min-base+)
                              (- expt-base +min-base+)))
            (offset (svref *log-3/4* (- log-base +min-base+)))
            (shift (- +log-expt-shift+)))
        (cond ((null three-quarters-p)
               `(ash (* ,exp ,multiplier) ,shift))
              ((constantp three-quarters-p)
               `(ash (+ (* ,exp ,multiplier) ,offset)
                     ,shift))
              (t
               `(ash (+ (* ,exp ,multiplier)
                        (if ,three-quarters-p
                            ,offset
                            0))
                     ,shift))))))

(defun ceiling-log-expt (log-base expt-base exp &optional three-quarters-p)
  (values (ceiling (+ (* exp (aref *log-expt*
                                   (- log-base +min-base+)
                                   (- expt-base +min-base+)))
                      (if three-quarters-p
                          (svref *log-3/4* (- log-base +min-base+))
                          0))
                   (ash 1 +log-expt-shift+))))

(define-compiler-macro ceiling-log-expt
    (&whole whole log-base expt-base exp &optional three-quarters-p)
  (if (or (not (constantp log-base))
          (not (constantp expt-base)))
      whole
      (let ((multiplier (aref *log-expt*
                              (- log-base +min-base+)
                              (- expt-base +min-base+)))
            (offset (svref *log-3/4* (- log-base +min-base+)))
            (divisor (ash 1 +log-expt-shift+)))
        (cond ((null three-quarters-p)
               `(values (ceiling (* ,exp ,multiplier) ,divisor)))
              ((constantp three-quarters-p)
               `(values (ceiling (+ (* ,exp ,multiplier) ,offset)
                                 ,divisor)))
              (t
               `(values (ceiling (+ (* ,exp ,multiplier)
                                    (if ,three-quarters-p
                                        ,offset
                                        0))
                                 ,divisor)))))))
