(in-package :one-more-re-nightmare)

(trivia:defun-match %prefix (re)
  "Find the constant string prefix of a regular expression."
  ((empty-set)
   (values '() (empty-string)))
  ((literal set)
   (if (csum-has-classes-p set)
       (values '() (empty-string))
       (values `((:literal ,set)) (empty-string))))
  ((tag-set tags)
   (values `((:tags ,tags)) (empty-string)))
  ((join r s)
   (multiple-value-bind (p1 s1)
       (%prefix r)
     (cond
       ((eq (empty-string) s1)
        ;; Haven't hit something not constant, keep searching.
        (multiple-value-bind (p2 s2)
            (%prefix s)
          (values (append p1 p2) s2)))
       (t
        (values p1 (join s1 s))))))
  ((alpha r _)
   (%prefix r))
  (_
   (values '() re)))

(defun prefix (re)
  (multiple-value-bind (prefix suffix)
      (%prefix re)
    (values prefix
            ;; Glue the tag map back on.
            (join (tag-set
                   (loop for (type tags) in prefix
                         when (eql type :tags)
                           append (loop for ((v r) . nil) in tags
                                        collect (cons (list v r) (list v r)))))
                  suffix))))
