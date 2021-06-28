(in-package :one-more-re-nightmare)

(defvar *tag-gensym-counter* 0)

(defun tag-gensym ()
  (incf *tag-gensym-counter*))

(defun gensym-position-assignments (set)
  "Replicate any assignments, turning T_n <- s for all s into T^r_n <- T_n for some arbitrary r"
  (loop for (variable replica source) in set
        collect (list variable (tag-gensym) (list variable replica))))

(defun unique-assignments (set)
  "Make assignments unique, turning T_n <- s for all s into T^r_n <- s"
  (loop for (variable replica source) in set
        collect (list variable (tag-gensym) source)))

(defun merge-tag-sets (set1 set2)
  (append (loop for (variable replica source) in set1
                unless (find (list variable replica) set2
                             :key (lambda (v2)
                                    (list (first v2) (second v2)))
                             :test #'equal)
                  collect (list variable replica source))
          set2))

(trivia:defun-match used-tags (re)
  ((tag-set s) (mapcar #'third s))
  ((or (either r s) (both r s) (join r s))
   (union (used-tags r) (used-tags s) :test #'equal))
  ((or (kleene r) (invert r))
   (used-tags r))
  ((grep vector _) (used-tags vector))
  ((alpha r history)
   (union (used-tags r) (used-tags history) :test #'equal))
  (_ '()))

(trivia:defun-match tags (re)
  ((tag-set s) s)
  ((or (either r s) (both r s) (join r s))
   (append (tags r) (tags s)))
  ((or (kleene r) (invert r))
   (tags r))
  ((grep r _) (tags r))
  ((alpha r _) (tags r))
  (_ '()))

(defun new-tags (new-re old-re)
  (loop with used = (used-tags new-re)
        for assignment
          in (set-difference (tags old-re) (tags new-re)
                             :test #'equal)
        for (variable replica nil) = assignment
        when (member (list variable replica) used :test #'equal)
          collect assignment))
 
(trivia:defun-match remove-tags (re)
  ((tag-set _) (empty-string))
  ((either r s) (either (remove-tags r) (remove-tags s)))
  ((both r s) (both (remove-tags r) (remove-tags s)))
  ((join r s) (join (remove-tags r) (remove-tags s)))
  ((kleene r) (kleene (remove-tags r)))
  ((invert r) (kleene (remove-tags r)))
  ((alpha r _) (remove-tags r))
  (_ re))

(trivia:defun-match unique-tags (re)
  ((tag-set set) (tag-set (unique-assignments set)))
  ((either r s) (either (unique-tags r) (unique-tags s)))
  ((both r s) (both (unique-tags r) (unique-tags s)))
  ((join r s) (join (unique-tags r) (unique-tags s)))
  ((kleene r) (kleene (unique-tags r)))
  ((invert r) (invert (unique-tags r)))
  ((alpha r old-tags)
   (unless (eq old-tags (empty-set))
     (error "Can't make unique tags with history"))
   (alpha (unique-tags r) old-tags))
  (_ re))
