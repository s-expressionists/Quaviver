(in-package #:quaviver/math)

(deftype word (arithmetic-size &optional (count 1))
  #+quaviver/bignum-elision
  (ecase arithmetic-size
    (32 (case count
          (1 `(unsigned-byte 32))
          (2 `(unsigned-byte 64))
          (otherwise `(simple-array (unsigned-byte 64) (,(ceiling count 2))))))
    (64 (if (eql count 1)
            `(unsigned-byte 64)
            `(simple-array (unsigned-byte 64) (,count))))
    (128 `(unsigned-byte ,(* arithmetic-size count))))
  #-quaviver/bignum-elision
  `(unsigned-byte ,(* arithmetic-size count)))

;;; Bignum elision
;;;
;;; From Dragonbox.
;;;
;;; TODO: Link to sources.
;;; TODO: Make parameter order more consistent based on width?
;;; TODO: Better name for WORD-HIGH operation.

(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 64))
                          (values (unsigned-byte 64) &optional))
                +/64-64)
         (ftype (function ((unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64))
                          (values (unsigned-byte 64) (unsigned-byte 64) &optional))
                +/128-64)
         (ftype (function ((unsigned-byte 32) (unsigned-byte 64))
                          (values (unsigned-byte 64) &optional))
                */32-64/hi64
                */32-64/lo64)
         (ftype (function ((unsigned-byte 64) (unsigned-byte 64))
                          (values (unsigned-byte 64) (unsigned-byte 64) &optional))
                */64-64)
         (ftype (function ((unsigned-byte 64) (unsigned-byte 64))
                          (values (unsigned-byte 64) &optional))
                */64-64/hi64
                */64-64/lo64)
         (ftype (function ((unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64))
                          (values (unsigned-byte 64) (unsigned-byte 64) &optional))
                */64-128/hi128
                */64-128/lo128)
         (inline +/64-64
                 +/128-64
                 */32-64/hi64
                 */32-64/lo64
                 */64-64
                 */64-64/hi64
                 */64-64/lo64
                 */64-128/hi128
                 */64-128/lo128))

(defun +/64-64 (x y)
  (ldb (byte 64 0) (+ x y)))

(defun +/128-64 (xh xl y)
  (let ((rl (+/64-64 xl y)))
    (values (+/64-64 xh (if (< rl xl) 1 0))
            rl)))

(defun */32-64/hi64 (x y)
  (+/64-64 (* x (ldb (byte 32 32) y))
           (ash (* x (ldb (byte 32 0) y))
                -32)))

(defun */32-64/lo64 (x y)
  ;; SBCL elides bignums.
  (ldb (byte 64 0) (* x y)))

(defun */64-64 (x y)
  (let* ((a (ldb (byte 32 32) x))
         (b (ldb (byte 32 0) x))
         (c (ldb (byte 32 32) y))
         (d (ldb (byte 32 0) y))
         (ac (* a c))
         (bc (* b c))
         (ad (* a d))
         (bd (* b d))
         (u (+ (ldb (byte 32 32) bd)
               (ldb (byte 32 0) ad)
               (ldb (byte 32 0) bc))))
    (declare ((unsigned-byte 32) a b c d)
             ((unsigned-byte 64) ac bc ad bd u))
    ;; (+ (ash ac 64) (ash ad 32) (ash bc 32) bd)
    (values (ldb (byte 64 0)
                 (+ ac
                    (ldb (byte 32 32) u)
                    (ldb (byte 32 32) ad)
                    (ldb (byte 32 32) bc)))
            (+/64-64 (ash (ldb (byte 32 0) u)
                          32)
                     (ldb (byte 32 0) bd)))))

(defun */64-64/hi64 (x y)
  (let* ((a (ldb (byte 32 32) x))
         (b (ldb (byte 32 0) x))
         (c (ldb (byte 32 32) y))
         (d (ldb (byte 32 0) y))
         (ac (* a c))
         (bc (* b c))
         (ad (* a d))
         (bd (* b d))
         (u (+ (ldb (byte 32 32) bd)
               (ldb (byte 32 0) ad)
               (ldb (byte 32 0) bc))))
    (declare ((unsigned-byte 32) a b c d)
             ((unsigned-byte 64) ac bc ad bd u))
    ;; (+ (ash ac 64) (ash ad 32) (ash bc 32) bd)
    (ldb (byte 64 0)
         (+ ac
            (ldb (byte 32 32) u)
            (ldb (byte 32 32) ad)
            (ldb (byte 32 32) bc)))))

(defun */64-64/lo64 (x y)
  ;; SBCL elides bignums.
  (ldb (byte 64 0) (* x y)))

(defun */64-128/hi128 (x yh yl)
  (multiple-value-bind (rh rl) (*/64-64 x yh)
    (+/128-64 rh rl (*/64-64/hi64 x yl))))

(defun */64-128/lo128 (x yh yl)
  (multiple-value-bind (rh rl) (*/64-64 x yl)
    (values (+/64-64 (*/64-64/lo64 x yh)
                     rh)
            rl)))

;;; Rest

(declaim (ftype (function ((unsigned-byte 64) (unsigned-byte 32))
                          (unsigned-byte 32))
                round-to-odd/32)
         (ftype (function (#+quaviver/bignum-elision (simple-array (unsigned-byte 64) (2))
                           #-quaviver/bignum-elision (unsigned-byte 128)
                           (unsigned-byte 64))
                          (unsigned-byte 64))
                round-to-odd/64)
         (ftype (function ((unsigned-byte 256) (unsigned-byte 128))
                          (unsigned-byte 128))
                round-to-odd/128)
         (ftype (function (fixnum) (unsigned-byte 64))
                expt10/32)
         (ftype (function (fixnum)
                          #+quaviver/bignum-elision (simple-array (unsigned-byte 64) (2))
                          #-quaviver/bignum-elision (unsigned-byte 128))
                expt10/64)
         (ftype (function (fixnum) (unsigned-byte 256))
                expt10/128)
         (ftype (function (fixnum fixnum fixnum &optional boolean) fixnum)
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
  #+quaviver/bignum-elision
  (let ((p (*/32-64/hi64 cp g)))
    (if (ldb-test (byte 31 1) p)
        (logior (ash p -32) 1)
        (ash p -32)))
  #-quaviver/bignum-elision
  (progn
    #-(or ecl cmucl) (%round-to-odd-1 g cp 32)
    #+(or ecl cmucl) (%round-to-odd-2 g cp 32)))

(defun round-to-odd/64 (g cp)
  #+quaviver/bignum-elision
  (multiple-value-bind (ph pl)
      (*/64-128/hi128 cp (aref g 0) (aref g 1))
    (if (ldb-test (byte 63 1) pl)
        (logior ph 1)
        ph))
  #-quaviver/bignum-elision
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
  #+quaviver/bignum-elision
  (let ((table (compute-expt +expt10/min-exponent/64+ +expt10/max-exponent/64+ 128)))
    (make-array (length table)
                :initial-contents
                (loop for x across table
                      ;; TODO: Test simple vector and svref too.
                      collect (make-array 2 :element-type '(unsigned-byte 64)
                                            :initial-contents
                                            (list (ldb (byte 64 64) x)
                                                  (ldb (byte 64 0) x))))))
  #-quaviver/bignum-elision
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
