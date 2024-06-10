(in-package #:quaviver/schubfach)

(defclass client (quaviver/ieee754:client
                  quaviver/integer-significand:client)
  ())

(defparameter +pow-10+
  #(#x81CEB32C4B43FCF5   ; -31
    #xA2425FF75E14FC32   ; -30
    #xCAD2F7F5359A3B3F   ; -29
    #xFD87B5F28300CA0E   ; -28
    #x9E74D1B791E07E49   ; -27
    #xC612062576589DDB   ; -26
    #xF79687AED3EEC552   ; -25
    #x9ABE14CD44753B53   ; -24
    #xC16D9A0095928A28   ; -23
    #xF1C90080BAF72CB2   ; -22
    #x971DA05074DA7BEF   ; -21
    #xBCE5086492111AEB   ; -20
    #xEC1E4A7DB69561A6   ; -19
    #x9392EE8E921D5D08   ; -18
    #xB877AA3236A4B44A   ; -17
    #xE69594BEC44DE15C   ; -16
    #x901D7CF73AB0ACDA   ; -15
    #xB424DC35095CD810   ; -14
    #xE12E13424BB40E14   ; -13
    #x8CBCCC096F5088CC   ; -12
    #xAFEBFF0BCB24AAFF   ; -11
    #xDBE6FECEBDEDD5BF   ; -10
    #x89705F4136B4A598   ;  -9
    #xABCC77118461CEFD   ;  -8
    #xD6BF94D5E57A42BD   ;  -7
    #x8637BD05AF6C69B6   ;  -6
    #xA7C5AC471B478424   ;  -5
    #xD1B71758E219652C   ;  -4
    #x83126E978D4FDF3C   ;  -3
    #xA3D70A3D70A3D70B   ;  -2
    #xCCCCCCCCCCCCCCCD   ;  -1
    #x8000000000000000   ;   0
    #xA000000000000000   ;   1
    #xC800000000000000   ;   2
    #xFA00000000000000   ;   3
    #x9C40000000000000   ;   4
    #xC350000000000000   ;   5
    #xF424000000000000   ;   6
    #x9896800000000000   ;   7
    #xBEBC200000000000   ;   8
    #xEE6B280000000000   ;   9
    #x9502F90000000000   ;  10
    #xBA43B74000000000   ;  11
    #xE8D4A51000000000   ;  12
    #x9184E72A00000000   ;  13
    #xB5E620F480000000   ;  14
    #xE35FA931A0000000   ;  15
    #x8E1BC9BF04000000   ;  16
    #xB1A2BC2EC5000000   ;  17
    #xDE0B6B3A76400000   ;  18
    #x8AC7230489E80000   ;  19
    #xAD78EBC5AC620000   ;  20
    #xD8D726B7177A8000   ;  21
    #x878678326EAC9000   ;  22
    #xA968163F0A57B400   ;  23
    #xD3C21BCECCEDA100   ;  24
    #x84595161401484A0   ;  25
    #xA56FA5B99019A5C8   ;  26
    #xCECB8F27F4200F3A   ;  27
    #x813F3978F8940985   ;  28
    #xA18F07D736B90BE6   ;  29
    #xC9F2C9CD04674EDF   ;  30
    #xFC6F7C4045812297   ;  31
    #x9DC5ADA82B70B59E   ;  32
    #xC5371912364CE306   ;  33
    #xF684DF56C3E01BC7   ;  34
    #x9A130B963A6C115D   ;  35
    #xC097CE7BC90715B4   ;  36
    #xF0BDC21ABB48DB21   ;  37
    #x96769950B50D88F5   ;  38
    #xBC143FA4E250EB32   ;  39
    #xEB194F8E1AE525FE   ;  40
    #x92EFD1B8D0CF37BF   ;  41
    #xB7ABC627050305AE   ;  42
    #xE596B7B0C643C71A   ;  43
    #x8F7E32CE7BEA5C70   ;  44
    #xB35DBF821AE4F38C)) ;  45

(defun round-to-odd (g cp)
  (let ((p (* g cp)))
    (logior (ldb (byte 64 64) p)
            (if (> (ldb (byte 64 0) p) 1) 1 0))))

(defmethod quaviver:float-decimal ((client client) (value single-float))
  (let* ((significand-size 24)
         (max-exponent 128)
         (min-exponent -125)
         (exponent-bias (+ max-exponent significand-size -2))
         (max-ieee-exponent (integer-length (1- (* 2 max-exponent))))
         (hidden-bit (ash 1 (1- significand-size))))
    (multiple-value-bind (ieee-significand ieee-exponent sign)
        (integer-decode-float value)
    (let* ((c (if t #+(or)(zerop ieee-exponent)
                  ieee-significand
                  (logior ieee-significand hidden-bit)))
           (q ieee-exponent);(zerop ieee-exponent)
               ;   (- 1 exponent-bias)
                ;  (- ieee-exponent exponent-bias)))
           (is-even (evenp c))
           (accept-lower is-even)
           (accept-upper is-even)
           (lower-boundary-is-closer (and (zerop ieee-significand)
                                         (> ieee-exponent 1)))
           (cb (* 4 c))
           (cbl (if lower-boundary-is-closer
                    (1- cb)
                    (- cb 2)))
           (cbr (+ cb 2))
           (k (ash (- (* q 1262611)
                      (if lower-boundary-is-closer 524031 0))
                   -22))
           (h (+ q 1 (ash (* (- k) 1741647) -19)))
           (pow10 (svref +pow-10+ (- 31 k)))
           (vbl (round-to-odd pow10 (ash cbl h)))
           (vb (round-to-odd pow10 (ash cb h)))
           (vbr (round-to-odd pow10 (ash cbr h)))
           (lower (if accept-lower
                      vbl
                      (1+ vbl)))
           (upper (if accept-lower
                      vbr
                      (1- vbr)))
           (s (ash vb -2)))
      (when (>= s 10)
        (let* ((sp (floor (/ s 10)))
               (up-inside (<= lower (* 40 sp)))
               (wp-inside (<= (* 40 (1+ sp)) upper)))
          (unless (eq up-inside wp-inside)
            (return-from quaviver:float-decimal
              (values (if wp-inside (1+ sp) sp)
                                 (1+ k)
                                 sign)))))
      (let ((u-inside (<= lower (* 4 s)))
            (w-inside (<= (* 4 (1+ s)) upper)))
        (unless (eq u-inside w-inside)
          (return-from quaviver:float-decimal
            (values (if w-inside (1+ s) s)
                    k
                    sign))))
      (let* ((mid (+ (* 4 s) 2))
             (round-up (or (not (zerop (ash vb (- mid))))
                           (and (= vb mid)
                                (logbitp s 0)))))
        (values (if round-up (1+ s) s)
                k
                sign))))))
