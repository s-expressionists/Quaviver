(in-package #:quaviver/math)

(deftype arithmetic-word (arithmetic-size &optional (count 1))
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

(declaim (ftype (function ((arithmetic-word 64) (integer 0 64))
                          (values (arithmetic-word 64) &optional))
                hi/64)
         (ftype (function ((arithmetic-word 64 2) (integer 0 64))
                          (values (arithmetic-word 64) &optional))
                hi/hi64/128)
         (ftype (function ((unsigned-byte 32) (arithmetic-word 32 2))
                          (unsigned-byte 32))
                round-to-odd/32-64)
         (ftype (function ((unsigned-byte 64) (arithmetic-word 64 2))
                          (unsigned-byte 64))
                round-to-odd/64-128)
         (ftype (function ((unsigned-byte 128) (arithmetic-word 128 2))
                          (unsigned-byte 128))
                round-to-odd/128-256)
         (ftype (function ((unsigned-byte 32) (arithmetic-word 32 2) &optional (integer 0 (32)))
                          (values (unsigned-byte 32) boolean &optional))
                floor-multiply/32-64q64)
         (ftype (function ((unsigned-byte 32) (arithmetic-word 32 2) &optional (integer 0 (32)))
                          (values boolean boolean &optional))
                floor-multiply/evenp/32-64q64)
         (ftype (function ((unsigned-byte 64) (arithmetic-word 64 2) &optional (integer 0 (64)))
                          (values (unsigned-byte 64) boolean &optional))
                floor-multiply/64-128q128)
         (ftype (function ((unsigned-byte 64) (arithmetic-word 64 2) &optional (integer 0 (64)))
                          (values boolean boolean &optional))
                floor-multiply/evenp/64-128q128)
         (ftype (function (fixnum) (arithmetic-word 32 2))
                expt10/32)
         (ftype (function (fixnum) (arithmetic-word 64 2))
                expt10/64)
         (ftype (function (fixnum) (arithmetic-word 128 2))
                expt10/128)
         (ftype (function (fixnum fixnum fixnum &optional boolean) fixnum)
                floor-log-expt ceiling-log-expt)
         (inline hi/64
                 hi/hi64/128
                 round-to-odd/32-64
                 round-to-odd/64-128
                 round-to-odd/128-256
                 floor-multiply/32-64q64
                 floor-multiply/evenp/32-64q64
                 floor-multiply/64-128q128
                 floor-multiply/evenp/64-128q128
                 expt10/32
                 expt10/64
                 expt10/128
                 floor-log-expt
                 ceiling-log-expt))

(defun hi/64 (x count)
  (ash x (- count 64)))

(defun hi/hi64/128 (x count)
  #+quaviver/bignum-elision
  (hi/64 (aref x 0) count)
  #-quaviver/bignum-elision
  (ash x (- count 128)))

(defmacro %round-to-odd-1 (cp g size)
  `(let ((p (* ,cp ,g)))
     (logior (ldb (byte ,size ,(ash size 1)) p)
             (if (> (ldb (byte ,size ,size) p) 1) 1 0))))

(defmacro %round-to-odd-2 (cp g size)
  `(let ((p (ash (* ,cp ,g) ,(- size))))
     (if (ldb-test (byte ,(1- size) 1) p)
         (logior (ash p ,(- size)) 1)
         (ash p ,(- size)))))

(defun round-to-odd/32-64 (cp g)
  #+quaviver/bignum-elision
  (let ((p (*/32-64/hi64 cp g)))
    (if (ldb-test (byte 31 1) p)
        (logior (ash p -32) 1)
        (ash p -32)))
  #+(and (not quaviver/bignum-elision) (not (or ecl cmucl)))
  (%round-to-odd-1 cp g 32)
  #+(and (not quaviver/bignum-elision) (or ecl cmucl))
  (%round-to-odd-2 cp g 32))

(defun round-to-odd/64-128 (cp g)
  #+quaviver/bignum-elision
  (multiple-value-bind (ph pl)
      (*/64-128/hi128 cp (aref g 0) (aref g 1))
    (if (ldb-test (byte 63 1) pl)
        (logior ph 1)
        ph))
  #-quaviver/bignum-elision
  (%round-to-odd-2 cp g 64))

(defun round-to-odd/128-256 (cp g)
  (%round-to-odd-2 cp g 128))

;;; The FLOOR-MULTIPLY operations return the same type as the initial argument.
;;;
;;; floor(2^pre * x * y)
;;;
;;; Also, pre-shift and x guarantee that 2^pre * x remains within the size of x.
;;;
;;; The pre-shift is <32 here unlike Dragonbox.
;;; Also, I've relaxed it to min 0.
;;; Relax it even more, e.g., no min?
;;;
;;; Also, zerop ldb vs not ldb-test triggers different compiler notes on
;;; SBCL.
;;; Investigate.
;;; They are currently inconsistent here.
;;; Also the LDB calls in the evenp versions seem like the size could be
;;; constrained.
;;;
;;; Based on
;;; https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L3064-L3068
;;; and
;;; https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L3121-L3125

(defmacro %floor-multiply/n-2nq2n (x y pre-shift size)
  `(let ((r (* (the (unsigned-byte ,size) (ash ,x ,pre-shift))
               ,y)))
     (values (ldb (byte ,size ,(* size 2)) r)      ; integer part
             (zerop (ldb (byte ,size ,size) r))))) ; integer-p

(defun floor-multiply/32-64q64 (x y &optional (pre-shift 0))
  #+quaviver/bignum-elision
  (let ((r (*/32-64/hi64 (ash x pre-shift) y)))
    (values (ldb (byte 32 32) r)             ; integer part
            (not (ldb-test (byte 32 0) r)))) ; integer-p
  #-quaviver/bignum-elision
  (%floor-multiply/n-2nq2n x y pre-shift 32))

(defun floor-multiply/64-128q128 (x y &optional (pre-shift 0))
  #+quaviver/bignum-elision
  (multiple-value-bind (rh rl)
      (*/64-128/hi128 (ash x pre-shift) (aref y 0) (aref y 1))
    (values rh (zerop rl))) ; integer part, integer-p
  #-quaviver/bignum-elision
  (%floor-multiply/n-2nq2n x y pre-shift 64))

;;; FLOOR-MULTIPLY/EVENP variants based on
;;; https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L3076-L3085
;;; and
;;; https://github.com/jk-jeon/dragonbox/blob/04bc662afe22576fd0aa740c75dca63609297f19/include/dragonbox/dragonbox.h#L3134-L3144

(defmacro %floor-multiply/evenp/n-2nq2n (x y pre-shift size)
  `(let* ((r (* ,x ,y)))
     (values (logbitp (- ,(* size 2) ,pre-shift) r) ; even-p
             (zerop (ldb (byte ,size (- ,size ,pre-shift)) r))))) ; integer-p

(defun floor-multiply/evenp/32-64q64 (x y &optional (pre-shift 0))
  #+quaviver/bignum-elision
  (let ((r (*/32-64/lo64 x y)))
    (values (logbitp (- 64 pre-shift) r)                    ; even-p
            (not (ldb-test (byte 32 (- 32 pre-shift)) r)))) ; integer-p
  #-quaviver/bignum-elision
  (%floor-multiply/evenp/n-2nq2n x y pre-shift 32))

(defun floor-multiply/evenp/64-128q128 (x y &optional (pre-shift 0))
  #+quaviver/bignum-elision
  (multiple-value-bind (rh rl)
      (*/64-128/lo128 x (aref y 0) (aref y 1))
    (values (logbitp (- 64 pre-shift) rh) ; even-p
            (and (zerop (ldb (byte (- 64 pre-shift) pre-shift) rh)) ; integer-p
                 (zerop (ldb (byte 64 (- 64 pre-shift)) rl)))))
  #-quaviver/bignum-elision
  (%floor-multiply/evenp/n-2nq2n x y pre-shift 64))

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
