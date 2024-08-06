(cl:in-package #:quaviver/compare)

(defgeneric interval-length (interval)
  (:method ((intervals list))
    (loop for interval in intervals
          sum (interval-length interval))))

(defgeneric make-iterator (interval))

(defgeneric iterator-next-p (iterator))

(defgeneric iterator-bits (iterator))

(defgeneric iterator-float (iterator))

(defgeneric iterator-integer (iterator base))

(defgeneric split-interval (interval count)
  (:method ((intervals list) count)
    (loop with width = (ceiling (interval-length intervals) count)
          for interval in intervals
          nconc (split-interval interval
                                (ceiling (interval-length interval) width)))))

(defclass interval ()
  ((float-type :accessor float-type
               :initarg :float-type
               :initform 'single-float)
   (coverage :accessor coverage
             :initarg :coverage
             :initform 1)))

(defclass iterator ()
  ((interval :reader iterator-interval
             :initarg :interval)
   (index :accessor iterator-index
          :initform -1)
   (bits :accessor iterator-bits
         :initform 0)
   (client :reader iterator-client
           :initform (make-instance 'quaviver/schubfach:client)
           :allocation :class)))

(defmethod iterator-float ((iterator iterator))
  (with-accessors ((bits iterator-bits)
                   (interval iterator-interval))
      iterator
    (with-accessors ((float-type float-type))
        interval
      (quaviver:bits-float float-type bits))))

(defmethod iterator-integer ((iterator iterator) base)
  (with-accessors ((bits iterator-bits)
                   (client iterator-client)
                   (interval iterator-interval))
      iterator
    (with-accessors ((float-type float-type))
        interval
      (quaviver:float-triple client base
                              (quaviver:bits-float float-type
                                                   bits)))))

(defclass bit-interval (interval)
  ((start :accessor start
          :initarg :start
          :initform 0)
   (end :accessor end
        :initarg :end
        :initform nil)))

(defmethod initialize-instance :after ((interval bit-interval) &rest initargs &key)
  (declare (ignore initargs))
  (with-accessors ((float-type float-type)
                   (start start)
                   (end end))
      interval
    (with-accessors ((storage-size quaviver:storage-size))
        float-type
      (unless end
        (setf end
              (1- (ash 1 storage-size)))))))

(defmethod print-object ((interval bit-interval) stream)
  (let ((width (ceiling (quaviver:storage-size (float-type interval)) 4)))
    (format stream "~:@<:bit :float-type ~s :start #x~v,'0x :end #x~v,'0x~:@>"
            (float-type interval)
            width
            (start interval)
            width
            (end interval))))

(defmethod interval-length ((interval bit-interval))
  (- (end interval) (start interval) -1))

(defclass bit-iterator (iterator)
  ())

(defmethod make-iterator ((interval bit-interval))
  (make-instance 'bit-iterator
                 :interval interval))

(defmethod iterator-next-p ((iterator bit-iterator))
  (with-accessors ((index iterator-index)
                   (bits iterator-bits)
                   (client iterator-client)
                   (interval iterator-interval))
      iterator
    (with-accessors ((float-type float-type)
                     (start start)
                     (end end)
                     (coverage coverage)
                     (interval-length interval-length))
        interval
      (incf index)
      (cond ((and (= coverage 1)
                  (< index interval-length))
             (if (zerop index)
                 (setf bits start)
                 (incf bits))
             t)
            ((and (< coverage 1)
                  (< index (* interval-length coverage)))
             (setf bits (+ (random (- end start -1)) start))
             t)
            (t
             nil)))))

(defmethod split-interval ((interval bit-interval) count)
  (when (< count 2)
    (return-from split-interval (list interval)))
  (with-accessors ((interval-length interval-length)
                   (start start)
                   (end end)
                   (float-type float-type)
                   (coverage coverage))
      interval
    (loop with width = (ceiling interval-length count)
          repeat count
          for sub-start = start then (1+ sub-end)
          for sub-end = (+ sub-start width -1)
          collect (make-instance 'bit-interval
                                 :float-type float-type
                                 :coverage coverage
                                 :start sub-start
                                 :end (min sub-end
                                           end)))))

(defclass bit-part-interval (interval)
  ((sign-start :accessor sign-start
               :initarg :sign-start
               :initform 0)
   (sign-end :accessor sign-end
             :initarg :sign-end
             :initform nil)
   (exponent-start :accessor exponent-start
                   :initarg :exponent-start
                   :initform 0)
   (exponent-end :accessor exponent-end
                 :initarg :exponent-end
                 :initform nil)
   (significand-start :accessor significand-start
                      :initarg :significand-start
                      :initform 0)
   (significand-end :accessor significand-end
                    :initarg :significand-end
                    :initform nil)))

(defmethod initialize-instance :after ((interval bit-part-interval) &rest initargs &key)
  (declare (ignore initargs))
  (with-accessors ((float-type float-type)
                   (sign-start sign-start)
                   (sign-end sign-end)
                   (exponent-start exponent-start)
                   (exponent-end exponent-end)
                   (significand-start significand-start)
                   (significand-end significand-end))
      interval
    (with-accessors ((storage-size quaviver:storage-size)
                     (significand-bytespec quaviver:significand-bytespec)
                     (exponent-bytespec quaviver:exponent-bytespec)
                     (sign-bytespec quaviver:sign-bytespec))
        float-type
      (unless sign-end
        (setf sign-end
              (1- (ash 1 (byte-size sign-bytespec)))))
      (unless exponent-end
        (setf exponent-end
              (1- (ash 1 (byte-size exponent-bytespec)))))
      (unless significand-end
        (setf significand-end
              (1- (ash 1 (byte-size significand-bytespec))))))))

(defmethod print-object ((interval bit-part-interval) stream)
  (let ((sign-width (ceiling (quaviver:sign-size (float-type interval)) 4))
        (exponent-width (ceiling (quaviver:exponent-size (float-type interval)) 4))
        (significand-width (ceiling (byte-size (quaviver:significand-bytespec (float-type interval))) 4)))
    (format stream "~:@<:bit-part :float-type ~s ~
                                  :sign-start #x~v,'0x :sign-end #x~v,'0x ~
                                  :exponent-start #x~v,'0x :exponent-end #x~v,'0x ~
                                  :significand-start #x~v,'0x :significand-end #x~v,'0x~:@>"
            (float-type interval)
            sign-width
            (sign-start interval)
            sign-width
            (sign-end interval)
            exponent-width
            (exponent-start interval)
            exponent-width
            (exponent-end interval)
            significand-width
            (significand-start interval)
            significand-width
            (significand-end interval))))

(defclass bit-part-iterator (iterator)
  ())

(defmethod make-iterator ((interval bit-part-interval))
  (make-instance 'bit-part-iterator
                 :interval interval))

(defmethod iterator-next-p ((iterator bit-part-iterator))
  (with-accessors ((index iterator-index)
                   (bits iterator-bits)
                   (client iterator-client)
                   (interval iterator-interval))
      iterator
    (with-accessors ((float-type float-type)
                     (sign-start sign-start)
                     (sign-end sign-end)
                     (exponent-start exponent-start)
                     (exponent-end exponent-end)
                     (significand-start significand-start)
                     (significand-end significand-end)
                     (end end)
                     (coverage coverage)
                     (interval-length interval-length))
        iterator
      (with-accessors ((sign-bytespec quaviver:sign-bytespec)
                       (exponent-bytespec quaviver:exponent-bytespec)
                       (significand-bytespec quaviver:significand-bytespec))
          float-type
        (cond ((and (= coverage 1)
                    (< index interval-length))
               (incf index)
               (if (zerop index)
                   (setf (ldb sign-bytespec bits)
                         sign-start
                         (ldb exponent-bytespec bits)
                         exponent-start
                         (ldb significand-bytespec bits)
                         significand-start)
                   (multiple-value-bind (q significand-index)
                       (floor index (- significand-end significand-start -1))
                     (multiple-value-bind (sign-index exponent-index)
                         (floor q (- exponent-end exponent-start -1))
                       (setf (ldb sign-bytespec bits)
                             (+ sign-start sign-index)
                             (ldb exponent-bytespec bits)
                             (+ exponent-start exponent-index)
                             (ldb significand-bytespec bits)
                             (+ significand-start significand-index)))))
               t)
              ((and (< coverage 1)
                    (< index (* interval-length coverage)))
               (incf index)
               (setf (ldb sign-bytespec bits)
                     (+ sign-start
                        (random (- sign-end sign-start -1)))
                     (ldb exponent-bytespec bits)
                     (+ exponent-start
                        (random (- exponent-end exponent-start -1)))
                     (ldb significand-bytespec bits)
                     (+ significand-start
                        (random (- significand-end significand-start -1))))
               t)
              (t
               nil))))))

(defmethod interval-length ((interval bit-part-interval))
  (* (- (sign-end interval) (sign-start interval) -1)
     (- (exponent-end interval) (exponent-start interval) -1)
     (- (significand-end interval) (significand-start interval) -1)))

(defmethod split-interval ((interval bit-part-interval) count)
  (when (< count 2)
    (return-from split-interval (list interval)))
  (with-accessors ((interval-length interval-length)
                   (sign-start sign-start)
                   (sign-end sign-end)
                   (exponent-start exponent-start)
                   (exponent-end exponent-end)
                   (significand-start significand-start)
                   (significand-end significand-end)
                   (float-type float-type)
                   (coverage coverage))
      interval
    (let ((sign-length (- sign-end sign-start -1))
          (exponent-length (- exponent-end exponent-start -1))
          (significand-length (- significand-end significand-start -1)))
      (loop with sign-width = (ceiling sign-length count)
            for sign-sub-start = sign-start
              then (1+ sign-sub-end)
            for sign-sub-end = (min (+ sign-sub-start sign-width -1)
                                    sign-end)
              then (min (+ sign-sub-start sign-width -1)
                        sign-end)
            while (<= sign-sub-start sign-end)
            nconc (loop with count = (ceiling count
                                              (ceiling sign-length
                                                                sign-width))
                        with exponent-width = (ceiling exponent-length
                                                       count)
                        for exponent-sub-start = exponent-start
                          then (1+ exponent-sub-end)
                        for exponent-sub-end = (min (+ exponent-sub-start
                                                       exponent-width -1)
                                                exponent-end)
                          then (min (+ exponent-sub-start
                                       exponent-width -1)
                                    exponent-end)
                        while (<= exponent-sub-start exponent-end)
                        nconc (loop with count = (ceiling count
                                                          (ceiling exponent-length
                                                                   exponent-width))
                                    with significand-width = (ceiling significand-length
                                                                      count)
                                    for significand-sub-start = significand-start
                                      then (1+ significand-sub-end)
                                    for significand-sub-end = (min (+ significand-sub-start
                                                                   significand-width -1)
                                                                significand-end)
                                      then (min (+ significand-sub-start
                                                   significand-width -1)
                                                significand-end)
                                    while (<= significand-sub-start significand-end)
                                    collect (make-instance 'bit-part-interval
                                                           :float-type float-type
                                                           :coverage coverage
                                                           :sign-start sign-sub-start
                                                           :sign-end sign-sub-end
                                                           :exponent-start exponent-sub-start
                                                           :exponent-end exponent-sub-end
                                                           :significand-start significand-sub-start
                                                           :significand-end significand-sub-end)))))))
