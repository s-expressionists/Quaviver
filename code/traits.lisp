(in-package #:quaviver)

(defmethod storage-size ((type (eql 'short-float)))
  #+clisp 25
  #-clisp 32)

(defmethod significand-bytespec ((type (eql 'short-float)))
  #+clisp (byte 16 0)
  #-clisp (byte 23 0))

(defmethod exponent-bytespec ((type (eql 'short-float)))
  #+clisp (byte 8 16)
  #-clisp (byte 8 23))

(defmethod sign-bytespec ((type (eql 'short-float)))
  #+clisp (byte 1 24)
  #-clisp (byte 1 31))

(defmethod nan-payload-bytespec ((type (eql 'short-float)))
  #+clisp (byte 15 0)
  #-clisp (byte 22 0))

(defmethod nan-type-bytespec ((type (eql 'short-float)))
  #+clisp (byte 1 15)
  #-clisp (byte 1 22))

(defmethod hidden-bit-p ((type (eql 'short-float)))
  t)

(defmethod exponent-bias ((type (eql 'short-float)))
  #+clisp 143
  #-clisp 150)

(defmethod max-exponent ((type (eql 'short-float)))
  #+clisp 110
  #-clisp 104)

(defmethod min-exponent ((type (eql 'short-float)))
  #+clisp -158
  #-clisp -172)

(defmethod arithmetic-size ((type (eql 'short-float)))
  32)

(defmethod storage-size ((type (eql 'single-float)))
  32)

(defmethod significand-bytespec ((type (eql 'single-float)))
  (byte 23 0))

(defmethod exponent-bytespec ((type (eql 'single-float)))
  (byte 8 23))

(defmethod sign-bytespec ((type (eql 'single-float)))
  (byte 1 31))

(defmethod nan-payload-bytespec ((type (eql 'single-float)))
  (byte 22 0))

(defmethod nan-type-bytespec ((type (eql 'single-float)))
  (byte 1 22))

(defmethod hidden-bit-p ((type (eql 'single-float)))
  t)

(defmethod exponent-bias ((type (eql 'single-float)))
  150)

(defmethod max-exponent ((type (eql 'single-float)))
  104)

(defmethod min-exponent ((type (eql 'single-float)))
  -172)

(defmethod arithmetic-size ((type (eql 'single-float)))
  32)

(defmethod storage-size ((type (eql 'double-float)))
  64)

(defmethod significand-bytespec ((type (eql 'double-float)))
  (byte 52 0))

(defmethod exponent-bytespec ((type (eql 'double-float)))
  (byte 11 52))

(defmethod sign-bytespec ((type (eql 'double-float)))
  (byte 1 63))

(defmethod nan-payload-bytespec ((type (eql 'double-float)))
  (byte 51 0))

(defmethod nan-type-bytespec ((type (eql 'double-float)))
  (byte 1 51))

(defmethod hidden-bit-p ((type (eql 'double-float)))
  t)

(defmethod exponent-bias ((type (eql 'double-float)))
  1075)

(defmethod max-exponent ((type (eql 'double-float)))
  971)

(defmethod min-exponent ((type (eql 'double-float)))
  -1126)

(defmethod arithmetic-size ((type (eql 'double-float)))
  64)

(defmethod storage-size ((type (eql 'long-float)))
  #+quaviver/long-float 80
  #-quaviver/long-float 64)

(defmethod significand-bytespec ((type (eql 'long-float)))
  #+quaviver/long-float (byte 64 0)
  #-quaviver/long-float (byte 52 0))

(defmethod exponent-bytespec ((type (eql 'long-float)))
  #+quaviver/long-float (byte 15 64)
  #-quaviver/long-float (byte 11 52))

(defmethod sign-bytespec ((type (eql 'long-float)))
  #+quaviver/long-float (byte 1 79)
  #-quaviver/long-float (byte 1 63))

(defmethod nan-payload-bytespec ((type (eql 'long-float)))
  #+quaviver/long-float (byte 63 0)
  #-quaviver/long-float (byte 51 0))

(defmethod nan-type-bytespec ((type (eql 'long-float)))
  #+quaviver/long-float (byte 1 63)
  #-quaviver/long-float (byte 1 51))

(defmethod hidden-bit-p ((type (eql 'long-float)))
  #+quaviver/long-float nil
  #-quaviver/long-float t)

(defmethod exponent-bias ((type (eql 'long-float)))
  #+quaviver/long-float 16446
  #-quaviver/long-float 1075)

(defmethod max-exponent ((type (eql 'long-float)))
  #+quaviver/long-float 16320
  #-quaviver/long-float 971)

(defmethod min-exponent ((type (eql 'long-float)))
  #+quaviver/long-float -16509
  #-quaviver/long-float -1126)

(defmethod arithmetic-size ((type (eql 'long-float)))
  #+quaviver/long-float 128
  #-quaviver/long-float 64)