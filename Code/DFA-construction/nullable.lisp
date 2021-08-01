(in-package :one-more-re-nightmare)

(defvar *gensym-assignments?* t)

(defun cached-nullable* (re)
  (if *gensym-assignments?*
      (cached-nullable re)
      (cached-nullable-no-gensym re)))

(defun (setf cached-nullable*) (value re)
  (if *gensym-assignments?*
      (setf (cached-nullable re) value)
      (setf (cached-nullable-no-gensym re) value)))

(defun nullable (re)
  "(language-of (nullable RE)) = (language-of (both RE (empty-string)))"
  (with-slot-consing (cached-nullable* re)
    (trivia:ematch re
      ((empty-string) (empty-string))
      ((literal _)    (empty-set))
      ((join r s)     (join   (nullable r) (nullable s)))
      ((either r s)   (let ((r* (nullable r)))
                        (if (typep r* '(or empty-string tag-set))
                            r*
                            (either r* (nullable s)))))
      ((kleene r)     (let ((r* (nullable r)))
                        (if (eq r* (empty-set))
                            (empty-string)
                            r*)))
      ((both r s)     (both (nullable r) (nullable s)))
      ((tag-set s)    (tag-set (gensym-position-assignments s)))
      ((invert r)     (invert (nullable r)))
      ((grep r _)     (nullable r))
      ((alpha r history) (either (nullable r) history)))))
