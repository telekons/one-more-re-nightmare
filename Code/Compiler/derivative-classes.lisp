(in-package :one-more-re-nightmare)

(defun merge-sets (sets1 sets2)
  "Produce a list of every subset of sets1 and sets2."
  ;; We hash-cons sets too, so EQ would suffice.
  (let ((sets (make-hash-table :test 'eq)))
    (loop for set1 in sets1
          do (loop for set2 in sets2
                   for intersection = (set-intersection set1 set2)
                   do (setf (gethash intersection sets) t)))
    (alexandria:hash-table-keys sets)))


(define-hash-consing-table *derivative-classes*)

(defun derivative-classes (re)
  "Produce a list of the 'classes' (sets) of characters that compiling the regular expression would have to dispatch on."
  (with-hash-consing (*derivative-classes* re)
    (trivia:ematch re
      ((literal set)  (list set (set-inverse set)))
      ((or (empty-string)
           (tag-set _))
       (list (make-instance 'negative-symbol-set :elements '())))
      ((join r s)
       (if (nullable r)
           (merge-sets (derivative-classes r)
                       (derivative-classes s))
           (derivative-classes r)))
      ((or (either r s) (both r s)
           (grep r s))
       (merge-sets (derivative-classes r)
                   (derivative-classes s)))
      ((or (kleene r) (invert r))
       (derivative-classes r))
      ((alpha r _)
       (derivative-classes r)))))
