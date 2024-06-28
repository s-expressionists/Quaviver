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
     (128 `(simple-array (unsigned-byte 64) (,(ash count 1)))))
   #-quaviver/bignum-elision
   `(unsigned-byte ,(* arithmetic-size count)))

;;; Bignum elision
;;;
;;; From Dragonbox.
;;; TODO Link

;;; Maybe U+/128, U*/128 and U*/128-upper64 or something.

(defmacro umul96-upper64 (x y)
  (let ((xg (gensym (string 'x)))
        (yg (gensym (string 'y))))
    `(let ((,xg ,x)
           (,yg ,y))
       (declare ((unsigned-byte 32) ,xg)
                ((unsigned-byte 64) ,yg))
       (ldb (byte 64 0)
            (+ (* ,xg (ldb (byte 32 32) ,yg))
               (ash (* ,xg (ldb (byte 32 0) ,yg)) -32))))))

(defmacro umul96-lower64 (x y)
  (let ((xg (gensym (string 'x)))
        (yg (gensym (string 'y))))
    `(let ((,xg ,x)
           (,yg ,y))
       (declare ((unsigned-byte 32) ,xg)
                ((unsigned-byte 64) ,yg))
       ;; Don't suppose there's a way to do overflow multiplication?
       ;; I think SBCL optimizes this properly!!
       (ldb (byte 64 0) (* ,xg ,yg)))))

(defmacro u128+ (hi lo n)
  (let ((hig (gensym (string 'hi)))
        (log (gensym (string 'lo)))
        (ng (gensym (string 'n))))
    `(let* ((,hig ,hi)
            (,log ,lo)
            (,ng ,n)
            (sum (ldb (byte 64 0) (+ ,log ,ng))))
       (declare ((unsigned-byte 64) ,hig ,log ,ng))
       (values (ldb (byte 64 0) (+ ,hig (if (< sum ,log) 1 0)))
               sum))))

(defmacro umul128 (x y)
  (let ((xg (gensym (string 'x)))
        (yg (gensym (string 'y))))
    `(let* ((,xg ,x)
            (,yg ,y)
            (a (ldb (byte 32 32) ,xg))
            (b (ldb (byte 32 0) ,xg))
            (c (ldb (byte 32 32) ,yg))
            (d (ldb (byte 32 0) ,yg))
            (ac (* a c))
            (bc (* b c))
            (ad (* a d))
            (bd (* b d))
            (intermediate (+ (ldb (byte 32 32) bd)
                             (ldb (byte 32 0) ad)
                             (ldb (byte 32 0) bc))))
       (declare ((unsigned-byte 64) ,xg ,yg))
       #+(or) (+ (ash ac 64) (ash bc 32) (ash ad 32) bd)
       (values (ldb (byte 64 0)
                    (+ ac
                       (ldb (byte 32 32) intermediate)
                       (ldb (byte 32 32) ad)
                       (ldb (byte 32 32) bc)))
               (ldb (byte 64 0)
                    (+ (ash (ldb (byte 32 0) intermediate)
                            32)
                       (ldb (byte 32 0) bd)))))))

#+(or)                                  ; tests
(progn
  (let ((x (ash 1 32))
        (y (ash 1 32)))
    (list (* x y) (umul128 x y)))

  (let ((x (ash 1 31))
        (y 2))
    (list (* x y) (umul128 x y)))

  (let ((x 123456789012345)
        (y 2))
    (list (* x y) (umul128 x y)))

  (let ((x 123456789012345)
        (y 123456789012345))
    (list (* x y) (umul128 x y))))

;;; TODO: Simplify these.
(defmacro umul128-upper64 (x y)
  (let ((xg (gensym (string 'x)))
        (yg (gensym (string 'y))))
    `(let* ((,xg ,x)
            (,yg ,y)
            (a (ldb (byte 32 32) ,xg))
            (b (ldb (byte 32 0) ,xg))
            (c (ldb (byte 32 32) ,yg))
            (d (ldb (byte 32 0) ,yg))
            (ac (* a c))
            (bc (* b c))
            (ad (* a d))
            (bd (* b d))
            (intermediate (+ (ldb (byte 32 32) bd)
                             (ldb (byte 32 0) ad)
                             (ldb (byte 32 0) bc))))
       (declare ((unsigned-byte 64) ,xg ,yg))
       #+(or) (+ (ash ac 64) (ash bc 32) (ash ad 32) bd) ; TODO: Test if faster
       (ldb (byte 64 0)
            (+ ac
               (ldb (byte 32 32) intermediate)
               (ldb (byte 32 32) ad)
               (ldb (byte 32 32) bc))))))

(defmacro umul192-upper128 (x yh yl)
  (let ((xg (gensym (string 'x)))
        (yhg (gensym (string 'yh)))
        (ylg (gensym (string 'yl))))
    `(let ((,xg ,x)
           (,yhg ,yh)
           (,ylg ,yl))
       (declare ((unsigned-byte 64) ,xg ,yhg ,ylg))
       (multiple-value-bind (rh rl) (umul128 ,xg ,yhg)
         (u128+ rh rl (umul128-upper64 ,xg ,ylg))))))

(defmacro umul192-lower128 (x yh yl)
  (let ((xg (gensym (string 'x)))
        (yhg (gensym (string 'yh)))
        (ylg (gensym (string 'yl))))
    `(let* ((,xg ,x)
            (,yhg ,yh)
            (,ylg ,yl)
            (high (ldb (byte 64 0) (* ,xg ,yhg))))
       (declare ((unsigned-byte 64) ,xg ,yhg ,ylg))
       (multiple-value-bind (rh rl) (umul128 ,xg ,ylg)
         (values (ldb (byte 64 0) (+ high rh))
                 rl)))))

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
  #-(or ecl cmucl) (%round-to-odd-1 g cp 32)
  #+(or ecl cmucl) (%round-to-odd-2 g cp 32))

(defun round-to-odd/64 (g cp)
  #+quaviver/bignum-elision
  (multiple-value-bind (ph pl)
      (umul192-upper128 cp (aref g 0) (aref g 1))
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
