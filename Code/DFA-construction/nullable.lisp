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
      ((either r s)   (either (nullable r) (nullable s)))
      ((repeat r min _ c) (let ((rn (if c (nullable r) (empty-set))))
                            (if (plusp min)
                                (empty-set)
                                (either rn (empty-string)))))
      ((both r s)     (both (nullable r) (nullable s)))
      ((tag-set s)    (tag-set (gensym-position-assignments s)))
      ((invert r)     (if (eq (nullable r) (empty-set))
                          (empty-string)
                          (empty-set)))
      ((grep r _)     (nullable r))
      ((alpha r history) (either (nullable r) history)))))
