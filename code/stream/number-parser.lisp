(in-package #:quaviver/stream)

(defun whitespace-char-p (x)
  (and (member x '(#\space #\tab #\page #\newline #\return))
       t))

(defun parse-number (string
                     &key (start 0) (end (length string)) (base 10) junk-allowed
                          ((:integer integerp) t) ((:ratio ratiop) t) ((:float floatp) t)
                          float-type (style #+ccl :common-lisp/ccl #-ccl :common-lisp)
                          (whitespace #'whitespace-char-p))
  (flet ((whitespace-or-eof-p (eofp index)
           (or (and eofp
                    (>= index end))
               (and (< index end)
                    (if (functionp whitespace)
                        (funcall whitespace (char string index))
                        (find (char string index) whitespace))))))
    (tagbody
     next
       (when (whitespace-or-eof-p nil start)
         (incf start)
         (go next)))
    (multiple-value-bind (value index)
        (quaviver:parse-number (getf *clients* style)
                               base string
                               start end
                               integerp ratiop floatp
                               float-type)
      (when (and (not junk-allowed)
                 (not (whitespace-or-eof-p t index)))
        (error 'quaviver.condition:invalid-character-error
               :found (char string index)))
      (values value index))))

(defun read-number (stream
                    &key (base 10) junk-allowed
                         ((:integer integerp) t) ((:ratio ratiop) t) ((:float floatp) t)
                         float-type (style :common-lisp) (whitespace #'whitespace-char-p))
  (flet ((whitespace-or-eof-p (eofp)
           (let ((char (peek-char nil stream nil)))
             (or (and eofp (null char))
                 (and char
                      (if (functionp whitespace)
                          (funcall whitespace char)
                          (find char whitespace)))))))
    (tagbody
     next
       (when (whitespace-or-eof-p nil)
         (read-char stream nil)
         (go next)))
    (let ((value (quaviver:read-number (getf *clients* style)
                                       base stream
                                       integerp ratiop floatp
                                       float-type)))
      (when (and (not junk-allowed)
                 (not (whitespace-or-eof-p t)))
        (error 'quaviver.condition:invalid-character-error
               :found (peek-char nil stream nil)))
      value)))
