(in-package :one-more-re-nightmare)

(trivia:defun-ematch nullable (re)
  "Could the regular expression match an empty string?"
  ((or (empty-string)
       (start-group _)
       (end-group _))
   t)
  ((literal _)    nil)
  ((join r s)     (and (nullable r) (nullable s)))
  ((either r s)   (or  (nullable r) (nullable s)))
  ((kleene _)     t)
  ((both r s)     (and (nullable r) (nullable s)))
  ((invert r)     (not (nullable r))))
