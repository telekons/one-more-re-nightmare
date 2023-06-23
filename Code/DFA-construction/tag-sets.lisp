(in-package :one-more-re-nightmare)

(defvar *tag-gensym-counter* 0)

(defun tag-gensym ()
  (incf *tag-gensym-counter*))

(defun gensym-position-assignments (set)
  "Replicate any assignments, turning T_n <- s for all s into T^r_n <- T_n for some arbitrary r"
  (loop for (target . source) in set
        for (variable nil) = target
        collect (cond
                  ((eql source 'nil)
                   (if *gensym-assignments?*
                       (cons (list variable (tag-gensym)) 'nil)
                       (cons target 'nil)))
                  ((not *gensym-assignments?*)
                   (cons target target))
                  (t
                   (cons (list variable (tag-gensym)) target)))))

(defun unique-assignments (set)
  "Make assignments unique, turning T_n <- s for all s into T^r_n <- s"
  (loop for ((variable nil) . source) in set
        collect (cons (list variable (tag-gensym)) source)))

(defun merge-tag-sets (set1 set2)
  (append (loop for assignment in set1
                for (target . source) = assignment
                unless (find (first target) set2 :key #'caar)
                  collect assignment)
          set2))

(defun used-tags (re)
  (with-slot-consing (cached-used-tags re)
    (trivia:match re
      ((tag-set s) (mapcar #'cdr s))
      ((or (either r s) (both r s) (join r s))
       (union (used-tags r) (used-tags s) :test #'equal))
      ((or (invert r) (repeat r _ _ _))
       (used-tags r))
      ((grep vector _) (used-tags vector))
      ((alpha r history)
       (union (used-tags r) (used-tags history) :test #'equal))
      (_ '()))))

(defun tags (re)
  (with-slot-consing (cached-tags re)
    (trivia:match re
      ((tag-set s) s)
      ((or (either r s) (both r s) (join r s))
       (union (tags r) (tags s) :test #'equal))
      ((or (invert r) (repeat r _ _ _))
       (tags r))
      ((grep r _) (tags r))
      ((alpha r _) (tags r))
      (_ '()))))

(defun keep-used-assignments (new-re assignments)
  (loop with used = (used-tags new-re)
        for assignment in assignments
        for (target . nil) = assignment
        when (member target used :test #'equal)
          collect assignment))
 
(defun remove-tags (re)
  (with-slot-consing (cached-removed-tags re)
    (trivia:match re
      ((tag-set _) (empty-string))
      ((either r s) (either (remove-tags r) (remove-tags s)))
      ((both r s) (both (remove-tags r) (remove-tags s)))
      ((join r s) (join (remove-tags r) (remove-tags s)))
      ((repeat r min max c) (repeat (remove-tags r) min max c))
      ((invert r) (invert (remove-tags r)))
      ((alpha r s)
       (either (remove-tags r)
               (if (eq s (empty-set))
                   (empty-set)
                   (empty-string))))
      ((grep r s) (grep (remove-tags r) (remove-tags s)))
      (_ re))))

(defun has-tags-p (re)
  (with-slot-consing (cached-has-tags-p re)
    (trivia:match re
      ((tag-set _) t)
      ((or (either r s) (both r s) (join r s) (alpha r s) (grep r s))
       (or (has-tags-p r) (has-tags-p s)))
      ((or (invert r) (repeat r _ _ _))
       (has-tags-p r))
      (_ nil))))

(defun unique-tags (re)
  ;; Return the same RE if we have no tags to replace.
  (unless (has-tags-p re) (return-from unique-tags re))
  (trivia:match re
    ((tag-set set)
     ;; Abstraction breakage here: we can't ever reuse a
     ;; TAG-SET by definition, so we don't try. One nasty case
     ;; Regrind found slows down about 16x if you do the naive
     ;; thing:
     #+(or) (tag-set (unique-assignments set))
     (let ((ts (make-instance 'tag-set)))
       (setf (slot-value ts 'substitutions) (unique-assignments set)
             (gethash (list set) *tag-set-table*) ts)))
    ((either r s) (either (unique-tags r) (unique-tags s)))
    ((both r s) (both (unique-tags r) (unique-tags s)))
    ((join r s) (join (unique-tags r) (unique-tags s)))
    ((invert r) (invert (unique-tags r)))
    ((repeat r min max c) (repeat (unique-tags r) min max c))
    ((alpha r old-tags)
     (unless (eq old-tags (empty-set))
       (error "Can't modify tags with history"))
     (alpha (unique-tags r) (unique-tags old-tags)))
    ((grep r s)
     (grep (unique-tags r) (unique-tags s)))
    (_ re)))
