(in-package #:quaviver)

(defun group-marker-p (index spec)
  (cond ((zerop index)
         nil)
        ((plusp index)
         (prog ((gid (position-if #'plusp spec)))
            (unless gid
              (return nil))
          next
            (cond ((< (1+ gid) (length spec))
                   (decf index (aref spec gid))
                   (incf gid)
                   (go next))
                  (t
                   (setf index (mod index (aref spec gid)))))
            (return (zerop index))))
        (t
         (prog ((gid (position-if #'minusp spec :from-end t)))
            (unless gid
              (return nil))
          next
            (cond ((plusp gid)
                   (decf index (aref spec gid))
                   (decf gid)
                   (go next))
                  (t
                   (setf index (mod index (aref spec gid)))))
            (return (zerop index))))))

(defun %write-smallnum-digits (base value stream fractional-position fractional-marker
                               digit-grouping group-marker
                               leading-zeros digit-char)
  (let* ((power (quaviver.math:count-digits base value))
         (fractional-position (or fractional-position power))
         (pos (- (min fractional-position 0)
                 leading-zeros))
         digit
         digits-written-p
         (zero-char (funcall digit-char 0 base)))
    (flet ((write-digit (ch)
             (when (and digits-written-p
                        digit-grouping
                        group-marker
                        (group-marker-p (- fractional-position pos)
                                        digit-grouping))
               (write-char group-marker stream))
             (write-char ch stream)
             (incf pos)
             (setf digits-written-p t)
             (when (and fractional-marker
                        (= pos fractional-position))
               (write-char fractional-marker stream))))
      (tagbody
         (when (and fractional-marker
                    (not (plusp fractional-position))
                    (zerop leading-zeros))
           (write-char fractional-marker stream))
       lead
         (when (plusp leading-zeros)
           (write-digit zero-char)
           (decf leading-zeros)
           (go lead))
       head
         (when (minusp pos)
           (write-digit zero-char)
           (go head))
       next
         (decf power)
         (when (plusp power)
           (multiple-value-setq (digit value)
             (floor value (expt base power)))
           (write-digit (funcall digit-char digit base))
           (go next))
         (write-digit (funcall digit-char value base))
       tail
         (when (and fractional-position
                    (< pos fractional-position))
           (write-digit zero-char)
           (go tail)))))
  value)

;; Algorithm by Harald Hanche-Olsen, sbcl-devel 2005-02-05
(defun %write-bignum-digits (base value stream fractional-position fractional-marker
                             digit-grouping group-marker
                             leading-zeros digit-char)
  (declare (type bignum value)
           (type fixnum base))
  (let* ((power (quaviver.math:count-digits base value))
         (fractional-position (or fractional-position power))
         (pos (- (min fractional-position 0)
                 leading-zeros))
         digit
         digits-written-p
         (zero-char (funcall digit-char 0 base)))
    (labels ((write-digit (ch)
               (when (and digits-written-p
                          digit-grouping
                          group-marker
                          (group-marker-p (- fractional-position pos)
                                          digit-grouping))
                 (write-char group-marker stream))
               (write-char ch stream)
               (incf pos)
               (setf digits-written-p t)
               (when (and fractional-marker
                          (= pos fractional-position))
                 (write-char fractional-marker stream)))
             (bisect (value k exactp)
               (declare (fixnum k))
               ;; VALUE is the number to bisect
               ;; K on initial entry BASE^(2^K) > VALUE
               ;; EXACTP is true if 2^K is the exact number of digits
               (cond ((zerop value)
                      (when exactp
                        (loop repeat (ash 1 k)
                              do (write-digit zero-char))))
                     ((zerop k)
                      (write-digit (funcall digit-char value base)))
                     (t
                      (decf k)
                      (multiple-value-bind (q r)
                          (truncate value (expt base (expt 2 k)))
                        ;; EXACTP is NIL only at the head of the
                        ;; initial number, as we don't know the number
                        ;; of digits there, but we do know that it
                        ;; doesn't get any leading zeros.
                        (bisect q k exactp)
                        (bisect r k (or exactp (plusp q))))))))
      (loop repeat leading-zeros
            do (write-digit zero-char))
      (when (and fractional-marker
                 (not (plusp fractional-position))
                 (zerop leading-zeros))
        (write-char fractional-marker stream))
      (when (minusp fractional-position)
        (loop repeat (- fractional-position)
              do (write-digit zero-char)))
      (bisect value
              (integer-length
               (quaviver.math:ceiling-log-expt
                base
                2
                (integer-length value)))
              nil)
      (when (>= fractional-position pos)
        (loop repeat (- fractional-position pos)
              do (write-digit zero-char)))))
  value)

(defun %write-pow2-digits (base value stream fractional-position fractional-marker
                           digit-grouping group-marker
                           leading-zeros digit-char)
  (let* ((size (1- (integer-length base)))
         (position (* size (ceiling (integer-length value) size)))
         (fractional-position (or fractional-position
                               (ceiling (integer-length value) size)))
         (pos (- (min fractional-position 0)
                 leading-zeros))
         digit
         digits-written-p
         (zero-char (funcall digit-char 0 base)))
    (flet ((write-digit (ch)
             (when (and digits-written-p
                        digit-grouping
                        group-marker
                        (group-marker-p (- fractional-position pos)
                                        digit-grouping))
               (write-char group-marker stream))
             (write-char ch stream)
             (incf pos)
             (setf digits-written-p t)
             (when (and fractional-marker
                        (= pos fractional-position))
               (write-char fractional-marker stream))))
      (tagbody
         (when (and fractional-marker
                    (minusp fractional-position)
                    (zerop leading-zeros))
           (write-char fractional-marker stream))
       lead
         (when (plusp leading-zeros)
           (write-digit zero-char)
           (decf leading-zeros)
           (go lead))
       head
         (when (minusp pos)
           (write-digit zero-char)
           (go head))
       next
         (decf position size)
         (unless (minusp position)
           (setf digit (ldb (byte size position) value))
           (write-digit (funcall digit-char digit base))
           (go next))
       tail
         (when (and fractional-position
                    (< pos fractional-position))
           (write-digit zero-char)
           (go tail))))))

(defun write-digits (base value stream
                     &key fractional-position fractional-marker
                          digit-grouping group-marker
                          leading-zeros (digit-char 'digit-char))
  (cond ((= (logcount base) 1)
         (%write-pow2-digits base value stream
                             fractional-position fractional-marker
                             digit-grouping group-marker
                             (or leading-zeros 0) digit-char))
        ((<= (integer-length value) 256)
         (%write-smallnum-digits base value stream
                                 fractional-position fractional-marker
                                 digit-grouping group-marker
                                 (or leading-zeros 0) digit-char))
        (t
         (%write-bignum-digits base value stream
                               fractional-position fractional-marker
                               digit-grouping group-marker
                               (or leading-zeros 0) digit-char))))
