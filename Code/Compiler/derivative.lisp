(in-package :one-more-re-nightmare)

(defun derivative (re set)
  "Compute the derivative of a regular expression with regards to the set (i.e. the regular expression should be matched after a character in the set is matched)."
  (trivia:ematch re
    ((or (empty-string) (empty-set) (tag-set _)) (empty-set))
    ((literal matching-set)
     (if (set-null (set-intersection matching-set set))
         (empty-set)
         (empty-string)))
    ((join r s)
     (let ((r* (derivative r set))
           (s* (derivative s set)))
       (either (join r* s) (join (nullable r) s*))))
    ((kleene r)
     (join (derivative r set) (kleene r)))
    ((either r s)
     (either (derivative r set) (derivative s set)))
    ((both r s)
     (both (derivative r set) (derivative s set)))
    ((invert r)
     (invert (derivative r set)))))

(defun derivative* (re sequence)
  (map 'nil
       (lambda (element)
         (setf re (derivative re (symbol-set element))))
       sequence)
  re)
