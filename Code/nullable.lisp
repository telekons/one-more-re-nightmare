(in-package :one-more-re-nightmare)

(trivia:defun-ematch nullable (re)
  "Could the regular expression match an empty string?"
  ((empty-string) t)
  ((literal set)  (set-null set))
  ((join r s)     (and (nullable r) (nullable s)))
  ((either r s)   (or  (nullable r) (nullable s)))
  ((kleene _)     t)
  ((both r s)     (and (nullable r) (nullable s)))
  ((invert r)     (not (nullable r))))
