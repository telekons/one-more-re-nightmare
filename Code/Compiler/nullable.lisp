(in-package :one-more-re-nightmare)

(trivia:defun-ematch nullable (re)
  "(language-of (nullable RE)) = (language-of (both RE (empty-string)))"
  ((empty-string) (empty-string))
  ((literal _)    (empty-set))
  ((join r s)     (both   (nullable r) (nullable s)))
  ((either r s)   (either (nullable r) (nullable s)))
  ((kleene _)     (empty-string))
  ((both r s)     (both (nullable r) (nullable s)))
  ((tag-set s)    (tag-set (gensym-position-assignments s)))
  ((invert r)     (invert (nullable r))))
