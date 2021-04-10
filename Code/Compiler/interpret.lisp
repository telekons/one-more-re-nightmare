(in-package :one-more-re-nightmare)

(defun interpret-regular-expression (expression input)
  (loop for char across input
        for position from 0
        do (when (eq expression (empty-set))
             (return-from interpret-regular-expression nil))
           (print expression)
           (setf expression (derivative expression (symbol-set char))))
  (if (eq expression (empty-set))
      nil
      (nullable expression)))
