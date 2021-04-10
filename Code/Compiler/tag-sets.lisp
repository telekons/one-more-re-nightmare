(in-package :one-more-re-nightmare)

(defvar *original-re*)

(defun tag-gensym ()
  *original-re*)

(defun gensym-position-assignments (set)
  "Replicate any assignments from position, turning T_n <- position into T^r_n <- T_n for some arbitrary r"
  (loop for (variable replica source) in set
        if (eql source 'position)
          collect (list variable (tag-gensym) (list variable replica))
        else
          collect (list variable replica source)))

(defun merge-tag-sets (set1 set2)
  (append (loop for (variable replica source) in set1
                unless (find variable set2 :key #'first)
                  collect (list variable replica source))
          set2))

(trivia:defun-match tags (re)
  ((tag-set s)
   (loop for (variable replica source) in s
         collect (list variable replica source)))
  ((or (either r s) (both r s) (join r s))
   (append (tags r) (tags s)))
  ((or (kleene r) (invert r))
   (tags r))
  (_ '()))

(defun new-tags (new-re old-re)
  (set-difference (tags new-re) (tags old-re)
                  :test #'equal))
