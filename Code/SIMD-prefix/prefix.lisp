(in-package :one-more-re-nightmare)

(trivia:defun-match prefix (re)
  "Find the constant string prefix of a regular expression."
  ((literal set)
   (values `((:literal ,set)) (empty-string)))
  ((tag-set tags)
   (values `((:tags ,tags)) (empty-string)))
  ((join r s)
   (multiple-value-bind (p1 s1)
       (prefix r)
     (cond
       ((eq (empty-string) s1)
        ;; Haven't hit something not constant, keep searching.
        (multiple-value-bind (p2 s2)
            (prefix s)
          (values (append p1 p2) s2)))
       (t
        (values p1 (join s1 s))))))
  (_
   (values '() re)))
