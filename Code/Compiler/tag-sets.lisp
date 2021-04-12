(in-package :one-more-re-nightmare)

(defvar *tag-gensym-counter* 0)

(defun tag-gensym ()
  (incf *tag-gensym-counter*))

(defun gensym-position-assignments (set)
  "Replicate any assignments, turning T_n <- s for all s into T^r_n <- T_n for some arbitrary r"
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

(trivia:defun-match remove-tags (re)
  ((tag-set _) (empty-string))
  ((either r s) (either (remove-tags r) (remove-tags s)))
  ((both r s) (both (remove-tags r) (remove-tags s)))
  ((join r s) (join (remove-tags r) (remove-tags s)))
  ((kleene r) (kleene (remove-tags r)))
  ((invert r) (kleene (remove-tags r)))
  (_ re))
