(in-package :one-more-re-nightmare)

(defun merge-sets (sets1 sets2)
  (remove-duplicates 
   (alexandria:map-product #'set-intersection sets1 sets2)
   :test #'symbol-set-equal))

(defun derivative-classes (re)
  (trivia:ematch re
    ((literal set)  (list set (set-inverse set)))
    ((empty-string) (list (make-instance 'negative-symbol-set :elements '())))
    ((join r s)
     (if (nullable r)
         (merge-sets (derivative-classes r)
                     (derivative-classes s))
         (derivative-classes r)))
    ((either r s)
     (merge-sets (derivative-classes r)
                 (derivative-classes s)))
    ((both r s)
     (merge-sets (derivative-classes r)
                 (derivative-classes s)))
    ((kleene r) (derivative-classes r))
    ((invert r) (derivative-classes r))))
    
