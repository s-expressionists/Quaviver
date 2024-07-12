(in-package #:quaviver.condition)

(defun floating-point-overflow (operation &rest operands)
  (error 'cl:floating-point-overflow
         :operation operation
         :operands operands))

(defun floating-point-underflow (operation &rest operands)
  (error 'cl:floating-point-underflow
         :operation operation
         :operands operands))
