(in-package :one-more-re-nightmare)

(defun interpret-regular-expression (regular-expression vector &key (start 0))
  "Try to match the vector starting at START with regular expression, returning the end of the match if successful, or NIL if it failed."
  (when (stringp regular-expression)
    (setf regular-expression (parse-regular-expression regular-expression)))
  (let ((position start)
        (end (length vector))
        (current-expression regular-expression))
    (loop
      (when (eq current-expression (empty-string))
        (return-from interpret-regular-expression position))
      (when (eq current-expression (empty-set))
        (return-from interpret-regular-expression))
      (when (>= position end)
        (return-from interpret-regular-expression
          (if (nullable current-expression)
              position
              0)))
      (setf current-expression
            (derivative current-expression (symbol-set (aref vector position))))
      (incf position))))
