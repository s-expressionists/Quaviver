(in-package #:quaviver)

(defvar *variable-map* nil)

(defvar *back-tag* nil)

(defvar *next-tag* nil)

(defvar *bindings* nil)

(defvar *current-backp* nil)

(defvar *character-test* 'equalp)

(defvar *definitions* nil)

(defun expand-ref (name)
  (getf *variable-map* name))

(defun expand-set (&rest properties)
  (when properties
    `((setf ,@(loop for (name value) on properties by #'cddr
                    collect (expand-ref name)
                    collect value)))))

(defgeneric expand-clause (variant key &optional items))

(defmethod expand-clause (variant (key cons) &optional items)
  (declare (ignore items))
  (expand-clause variant (car key) (cdr key)))

(defmethod expand-clause ((variant (eql 'read-number)) (key (eql :digits)) &optional items)
  (alexandria:with-gensyms
      (value count leading-zero)
    (destructuring-bind (name &key ignore ((:leading-zeros leading-zeros-p) t))
        items
      `((multiple-value-bind (,value ,count ,leading-zero)
            (read-digits ,(expand-ref (ecase name
                                        (:integral :integral-base)
                                        (:fractional :fractional-base)
                                        (:divisor :divisor-base)
                                        (:exponent :exponent-base)))
                         ,(expand-ref :stream)
                         :ignore ,(if (or (null ignore)
                                          (stringp ignore))
                                      ignore
                                      `(funcall ,ignore ,(expand-ref :client))))
          (cond ((or (plusp ,count) (plusp ,leading-zero))
                 ,@(when *current-backp*
                     `((setf ,(car *current-backp*) nil))))
                ,@(when *current-backp*
                    `((,(car *current-backp*)
                       (go ,*back-tag*))))
                (t
                 (error 'quaviver.condition:missing-digits-error
                        :part ,name
                        :found (peek-char nil ,(expand-ref :stream) nil))))
          ,@(unless leading-zeros-p
              `((when (or (and (> ,leading-zero 1)
                               (zerop ,value))
                          (and (plusp ,leading-zero)
                               (not (zerop ,value))))
                  (error 'quaviver.condition:invalid-leading-zeros-error
                         :part ,name
                         :count ,leading-zero))))
          ,@(expand-set name value
                        (ecase name
                          (:integral :integral-count)
                          (:fractional :fractional-count)
                          (:divisor :divisor-count)
                          (:exponent :exponent-count))
                        count
                        (ecase name
                          (:integral :integral-leading-zero)
                          (:fractional :fractional-leading-zero)
                          (:divisor :divisor-leading-zero)
                          (:exponent :exponent-leading-zero))
                        leading-zero)
          (go ,*next-tag*))))))

(defmethod expand-clause ((variant (eql 'parse-number)) (key (eql :digits)) &optional items)
  (alexandria:with-gensyms
      (value start count leading-zero)
    (destructuring-bind (name &key ignore ((:leading-zeros leading-zeros-p) t))
        items
      `((multiple-value-bind (,value ,start ,count ,leading-zero)
            (parse-digits ,(expand-ref (ecase name
                                         (:integral :integral-base)
                                         (:fractional :fractional-base)
                                         (:divisor :divisor-base)
                                         (:exponent :exponent-base)))
                          ,(expand-ref :sequence)
                          :start ,(expand-ref :start)
                          :end ,(expand-ref :end)
                          :ignore ,(if (or (null ignore)
                                           (stringp ignore))
                                       ignore
                                       `(funcall ,ignore ,(expand-ref :client))))
          (cond ((or (plusp ,count) (plusp ,leading-zero))
                 ,@(when *current-backp*
                     `((setf ,(car *current-backp*) nil))))
                ,@(when *current-backp*
                    `((,(car *current-backp*)
                       (go ,*back-tag*))))
                (t
                 (error 'quaviver.condition:missing-digits-error
                        :part ,name
                        :found (and (< ,(expand-ref :start)
                                       ,(expand-ref :end))
                                    (char ,(expand-ref :sequence)
                                          ,(expand-ref :start))))))
          ,@(unless leading-zeros-p
              `((when (or (and (> ,leading-zero 1)
                               (zerop ,value))
                          (and (plusp ,leading-zero)
                               (not (zerop ,value))))
                  (error 'quaviver.condition:invalid-leading-zeros-error
                         :part ,name
                         :count ,leading-zero))))
          ,@(expand-set :start start
                        name value
                        (ecase name
                          (:integral :integral-count)
                          (:fractional :fractional-count)
                          (:divisor :divisor-count)
                          (:exponent :exponent-count))
                        count
                        (ecase name
                          (:integral :integral-leading-zero)
                          (:fractional :fractional-leading-zero)
                          (:divisor :divisor-leading-zero)
                          (:exponent :exponent-leading-zero))
                        leading-zero)
          (go ,*next-tag*))))))

(defmethod expand-clause ((variant (eql 'read-number)) (key (eql :digits?)) &optional items)
  (alexandria:with-gensyms
      (value count leading-zero)
    (destructuring-bind (name &key ignore ((:leading-zeros leading-zeros-p) t))
        items
      `((multiple-value-bind (,value ,count ,leading-zero)
            (read-digits ,(expand-ref (ecase name
                                        (:integral :integral-base)
                                        (:fractional :fractional-base)
                                        (:divisor :divisor-base)
                                        (:exponent :exponent-base)))
                         ,(expand-ref :stream)
                         :ignore ,(if (or (null ignore)
                                          (stringp ignore))
                                      ignore
                                      `(funcall ,ignore ,(expand-ref :client))))
          (declare (ignorable ,count ,leading-zero))
          ,@(when *current-backp*
              `((when (or (plusp ,count) (plusp ,leading-zero))
                  (setf ,(car *current-backp*) nil))))
          ,@(unless leading-zeros-p
              `((when (or (and (> ,leading-zero 1)
                               (zerop ,value))
                          (and (plusp ,leading-zero)
                               (not (zerop ,value))))
                  (error 'quaviver.condition:invalid-leading-zeros-error
                         :part ,name
                         :count ,leading-zero))))
          ,@(expand-set name value
                        (ecase name
                          (:integral :integral-count)
                          (:fractional :fractional-count)
                          (:divisor :divisor-count)
                          (:exponent :exponent-count))
                        count
                        (ecase name
                          (:integral :integral-leading-zero)
                          (:fractional :fractional-leading-zero)
                          (:divisor :divisor-leading-zero)
                          (:exponent :exponent-leading-zero))
                        leading-zero)
          ,@(when *next-tag*
              `((go ,*next-tag*))))))))

(defmethod expand-clause ((variant (eql 'parse-number)) (key (eql :digits?)) &optional items)
  (alexandria:with-gensyms
      (value start count leading-zero)
    (destructuring-bind (name &key ignore ((:leading-zeros leading-zeros-p) t))
        items
      `((multiple-value-bind (,value ,start ,count ,leading-zero)
            (parse-digits ,(expand-ref (ecase name
                                         (:integral :integral-base)
                                         (:fractional :fractional-base)
                                         (:divisor :divisor-base)
                                         (:exponent :exponent-base)))
                          ,(expand-ref :sequence)
                          :start ,(expand-ref :start)
                          :end ,(expand-ref :end)
                          :ignore ,(if (or (null ignore)
                                           (stringp ignore))
                                       ignore
                                       `(funcall ,ignore ,(expand-ref :client))))
          (declare (ignorable ,count ,leading-zero))
          ,@(when *current-backp*
              `((when (or (plusp ,count) (plusp ,leading-zero))
                  (setf ,(car *current-backp*) nil))))
          ,@(unless leading-zeros-p
              `((when (or (and (> ,leading-zero 1)
                               (zerop ,value))
                          (and (plusp ,leading-zero)
                               (not (zerop ,value))))
                  (error 'quaviver.condition:invalid-leading-zeros-error
                         :part ,name
                         :count ,leading-zero))))
          ,@(expand-set :start start
                        name value
                        (ecase name
                          (:integral :integral-count)
                          (:fractional :fractional-count)
                          (:divisor :divisor-count)
                          (:exponent :exponent-count))
                        count
                        (ecase name
                          (:integral :integral-leading-zero)
                          (:fractional :fractional-leading-zero)
                          (:divisor :divisor-leading-zero)
                          (:exponent :exponent-leading-zero))
                        leading-zero)
          (go ,*next-tag*))))))

(defmethod expand-clause (variant (key (eql :sequence)) &optional items)
  `(,@(loop with outer-next-tag = *next-tag*
            for (item . rest) on items
            for pos from 0
            for *next-tag* = (if rest
                                 (alexandria:make-gensym '#:next)
                                 outer-next-tag)
            nconc (expand-clause variant item)
            when rest
              collect *next-tag*)))

(defmethod expand-clause (variant (key (eql :sequence?)) &optional items)
  (let ((backp (alexandria:make-gensym '#:backp))
        (outer-next-tag *next-tag*)
        (next-tag (alexandria:make-gensym '#:next)))
    (push `(,backp ,t) *bindings*)
    `(,@(loop with *back-tag* = *next-tag*
              with *current-backp* = (list* backp *current-backp*)
              for (item . rest) on items
              for *next-tag* = (if rest (alexandria:make-gensym '#:next) next-tag)
              nconc (expand-clause variant item)
              when rest
                collect *next-tag*)
      ,next-tag
        ,@(when *current-backp*
            `((unless ,backp
                (setf ,(car *current-backp*) nil))))
        (go ,outer-next-tag))))

(defmethod expand-clause (variant (key (eql :alternate)) &optional items)
  (let ((backp (alexandria:make-gensym '#:backp))
        (outer-next-tag *next-tag*)
        (next-tag (alexandria:make-gensym '#:next)))
    (push `(,backp ,t) *bindings*)
    `(,@(loop with *next-tag* = next-tag
              with *current-backp* = (list* backp *current-backp*)
              for (item . rest) on items
              for *back-tag* = (alexandria:make-gensym '#:back)
              while rest
              nconc (expand-clause variant item)
              collect *back-tag*
              when (cdr rest)
                collect `(setf ,(car *current-backp*) t))
        ,@(expand-clause variant (car (last items)))
      ,next-tag
        ,@(when *current-backp*
            `((unless ,backp
                (setf ,(car *current-backp*) nil))))
        (go ,outer-next-tag))))

(defmethod expand-clause (variant (key (eql :alternate?)) &optional items)
  (let ((*current-backp* (list* (alexandria:make-gensym '#:backp) *current-backp*))
        (outer-next-tag *next-tag*)
        (*next-tag* (alexandria:make-gensym '#:next)))
    (push `(,(car *current-backp*) ,t) *bindings*)
    `(,@(loop for (item . rest) on items
              for *back-tag* = (if rest (alexandria:make-gensym '#:back) outer-next-tag)
              nconc (expand-clause variant item)
              when rest
                collect *back-tag*
              when (cdr rest)
                collect `(setf ,(car *current-backp*) t))
      ,*next-tag*
        ,@(when  (cdr *current-backp*)
            `((unless ,(car *current-backp*)
                (setf ,(cadr *current-backp*) nil))))
        (go ,outer-next-tag))))

(defmethod expand-clause ((variant (eql 'read-number)) (expected character) &optional items)
  (declare (ignore items))
  `((setf ,(expand-ref :character) (peek-char nil ,(expand-ref :stream) nil))
    (cond ((,*character-test* ,(expand-ref :character) ,expected)
           (read-char ,(expand-ref :stream) nil)
           ,@(when *current-backp*
               `((setf ,(car *current-backp*) nil)))
           (go ,*next-tag*))
          ,@(when *current-backp*
              `((,(car *current-backp*)
                 (go ,*back-tag*))))
          (t
           (error 'quaviver.condition:invalid-character-error
                  :found ,(expand-ref :character)
                  :expected ,expected)))))

(defmethod expand-clause ((variant (eql 'parse-number)) (expected character) &optional items)
  (declare (ignore items))
  `((setf ,(expand-ref :character)
          (and (< ,(expand-ref :start) ,(expand-ref :end))
               (char ,(expand-ref :sequence) ,(expand-ref :start))))
    (cond ((,*character-test* ,(expand-ref :character) ,expected)
           (incf ,(expand-ref :start))
           ,@(when *current-backp*
               `((setf ,(car *current-backp*) nil)))
           (go ,*next-tag*))
          ,@(when *current-backp*
              `((,(car *current-backp*)
                 (go ,*back-tag*))))
          (t
           (error 'quaviver.condition:invalid-character-error
                  :found ,(expand-ref :character)
                  :expected ,expected)))))

(defmethod expand-clause (variant (key string) &optional items)
  (declare (ignore items))
  (expand-clause variant :sequence (coerce key 'list)))

(defmethod expand-clause (variant (key (eql :assert)) &optional items)
  (declare (ignore variant))
  (destructuring-bind (name-or-func &optional value-or-message)
      items
    (if (keywordp name-or-func)
        (let ((ref (expand-ref name-or-func)))
          `((cond ((eql ,ref ,value-or-message)
                   (go ,*next-tag*))
                  ,@(when *current-backp*
                      `((,(car *current-backp*)
                         (go ,*back-tag*))))
                  (t
                   (error 'quaviver.condition:invalid-property-error
                          :name ,name-or-func
                          :value ,ref
                          :expected ,value-or-message)))))
        `((cond ((funcall ,name-or-func ,(expand-ref :client))
                 (go ,*next-tag*))
                ,@(when *current-backp*
                    `((,(car *current-backp*)
                       (go ,*back-tag*))))
                (t
                 (error 'quaviver.condition:assertion-failed-error
                        :message ,value-or-message)))))))

(defmethod expand-clause (variant (key (eql :set)) &optional items)
  `(,@(apply #'expand-set items)
    (go ,*next-tag*)))

(defmethod expand-clause (variant (name symbol) &optional items)
  (declare (ignore items))
  (expand-clause variant (getf *definitions* name)))

(defun expand-parser (variant definitions items integerp ratiop floatp &rest map)
  (alexandria:with-gensyms
      (integral integral-count integral-leading-zero integral-base
       fractional fractional-count fractional-leading-zero fractional-base
       divisor divisor-count divisor-leading-zero divisor-base
       exponent exponent-count exponent-leading-zero exponent-base
       exponent-sign sign character code payload)
    (let* ((*next-tag* (alexandria:make-gensym '#:next))
           (*back-tag* nil)
           (*variable-map* `(:integral ,integral
                             :integral-count ,integral-count
                             :integral-leading-zero ,integral-leading-zero
                             :integral-base ,integral-base
                             :fractional ,fractional
                             :fractional-count ,fractional-count
                             :fractional-leading-zero ,fractional-leading-zero
                             :fractional-base ,fractional-base
                             :divisor ,divisor
                             :divisor-count ,divisor-count
                             :divisor-leading-zero ,divisor-leading-zero
                             :divisor-base ,divisor-base
                             :exponent-sign ,exponent-sign
                             :exponent ,exponent
                             :exponent-count ,exponent-count
                             :exponent-leading-zero ,exponent-leading-zero
                             :exponent-base ,exponent-base
                             :sign ,sign
                             :character ,character
                             :code ,code
                             :payload ,payload
                             ,@map))
           (*current-backp* nil)
           (*bindings* nil)
           (*definitions* definitions)
           (expanded-clauses (expand-clause variant :sequence items)))
      `(prog ((,integral 0)
              (,integral-count 0)
              (,integral-leading-zero 0)
              (,integral-base base)
              (,fractional 0)
              (,fractional-count 0)
              (,fractional-leading-zero 0)
              (,fractional-base base)
              (,divisor 1)
              (,divisor-count 0)
              (,divisor-leading-zero 0)
              (,divisor-base base)
              (,exponent-sign 1)
              (,exponent 0)
              (,exponent-count 0)
              (,exponent-leading-zero 0)
              (,exponent-base base)
              (,sign 1)
              (,character nil)
              (,code nil)
              (,payload 0)
              ,@*bindings*)
          (declare (ignorable ,integral ,integral-count ,integral-leading-zero ,integral-base
                              ,fractional ,fractional-count ,fractional-leading-zero ,fractional-base
                              ,divisor ,divisor-count, divisor-leading-zero ,divisor-base
                              ,exponent-sign ,exponent ,exponent-count ,exponent-leading-zero ,exponent-base
                              ,sign ,character))
          ,@expanded-clauses
        ,*next-tag*
          (cond (,code
                 (return (quaviver:integer-float ,(expand-ref :client)
                                                 ,(expand-ref :float-type)
                                                 ,(expand-ref :base)
                                                 ,payload
                                                 ,code
                                                 ,sign)))
                ,@(when integerp
                    `(((and ,(expand-ref :integer)
                            (or (plusp ,sign)
                                (not (zerop ,integral))))
                       (return (* ,sign ,integral)))))
                ,@(when ratiop
                    `(((and ,(expand-ref :ratio)
                            (or (plusp ,sign)
                                (not (zerop ,integral))))
                       (return (/ (* ,sign ,integral) ,divisor)))))
                ,@(when floatp
                    `((,(expand-ref :float)
                       (return (quaviver:integer-float ,(expand-ref :client)
                                                       ,(expand-ref :float-type)
                                                       ,(expand-ref :base)
                                                       (if (zerop ,integral)
                                                           ,fractional
                                                           (+ (* (expt ,(expand-ref :base)
                                                                       (+ ,fractional-count
                                                                          ,fractional-leading-zero))
                                                                 ,integral)
                                                              ,fractional))
                                                       (- (* ,exponent-sign ,exponent)
                                                          ,fractional-count
                                                          ,fractional-leading-zero)
                                                       ,sign)))))
                (t
                 (error 'parse-error)))))))

(defmacro define-number-parser ((&key client
                                      base
                                      ((:integer integerp) t)
                                      ((:ratio ratiop) t)
                                      ((:float floatp) t)
                                      (float-type '*read-default-float-format*)
                                      ((:case casep) nil))
                                definitions
                                &body items)
  (let ((*character-test* (if casep 'equal 'equalp)))
    `(progn
       (defmethod read-number
           (,(if client
                 `(client ,client)
                 'client)
            ,(if base
                 `(base (eql ,base))
                 'base)
            stream
            &optional (integerp t) (ratiop t) (floatp t)
                      float-type)
         (declare (ignorable integerp ratiop floatp))
         (unless float-type
           (setf float-type ,float-type))
         ,(expand-parser 'read-number definitions items
                         integerp ratiop floatp
                         :client 'client
                         :stream 'stream
                         :base 'base
                         :float-type 'float-type
                         :integer 'integerp
                         :ratio 'ratiop
                         :float 'floatp))

       (defmethod parse-number
           (,(if client
                 `(client ,client)
                 'client)
            ,(if base
                 `(base (eql ,base))
                 'base)
            sequence
            &optional (start 0) (end (length sequence))
                      (integerp t) (ratiop t) (floatp t)
                      float-type)
         (declare (ignorable integerp ratiop floatp))
         (unless float-type
           (setf float-type ,float-type))
         (values ,(expand-parser 'parse-number definitions items
                                 integerp ratiop floatp
                                 :client 'client
                                 :sequence 'sequence
                                 :start 'start
                                 :end 'end
                                 :base 'base
                                 :float-type 'float-type
                                 :integer 'integerp
                                 :ratio 'ratiop
                                 :float 'floatp)
                 start)))))
