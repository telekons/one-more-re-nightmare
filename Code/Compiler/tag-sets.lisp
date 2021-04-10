(in-package :one-more-re-nightmare)

(defvar *tag-gensym-counter* 0)

(defun tag-gensym ()
  (incf *tag-gensym-counter*))

(defun gensym-position-assignments (set)
  "Replicate any assignments from position, turning T_n <- position into T^r_n <- T_n for some arbitrary r"
  (loop for (variable replica source) in set
        collect (list variable (tag-gensym) (list variable replica))))

(defun merge-tag-sets (set1 set2)
  (append (loop for (variable replica source) in set1
                unless (find variable set2 :key #'first)
                  collect (list variable replica source))
          set2))

(trivia:defun-match tags (re)
  ((tag-set s) s)
  ((or (either r s) (both r s) (join r s))
   (append (tags r) (tags s)))
  ((or (kleene r) (invert r))
   (tags r))
  (_ '()))

(defun new-tags (new-re old-re)
  (set-difference (tags old-re) (tags new-re)
                  :test #'equal))
