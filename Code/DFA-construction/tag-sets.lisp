(in-package :one-more-re-nightmare)

(defvar *tag-gensym-counter* 0)

(defun tag-gensym ()
  (incf *tag-gensym-counter*))

(defun gensym-position-assignments (set)
  "Replicate any assignments, turning T_n <- s for all s into T^r_n <- T_n for some arbitrary r"
  (loop for (target . source) in set
        for (variable nil) = target
        collect (cond
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
      ((or (kleene r) (invert r))
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
      ((or (kleene r) (invert r))
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

(defun new-tags (new-re old-re)
  (let* ((*gensym-assignments?* nil))
    (trivia:ematch (nullable old-re)
      ((tag-set s) (keep-used-assignments new-re s))
      ((empty-string) '())
      ((empty-set)
       (let ((assignments (set-difference (tags old-re) (tags new-re)
                                          :test #'equal)))
         (keep-used-assignments new-re assignments))))))
 
(defun remove-tags (re)
  (with-slot-consing (cached-removed-tags re)
    (trivia:match re
      ((tag-set _) (empty-string))
      ((either r s) (either (remove-tags r) (remove-tags s)))
      ((both r s) (both (remove-tags r) (remove-tags s)))
      ((join r s) (join (remove-tags r) (remove-tags s)))
      ((kleene r) (kleene (remove-tags r)))
      ((invert r) (kleene (remove-tags r)))
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
      ((or (kleene r) (invert r))
       (has-tags-p r))
      (_ nil))))

(defvar *allow-alpha* t)
(defun map-tags (f re)
  ;; Return the same RE if we have no tags to replace.
  (unless (has-tags-p re)
    (return-from map-tags re))
  (trivia:match re
    ((tag-set set) (tag-set (funcall f set)))
    ((either r s) (either (map-tags f r) (map-tags f s)))
    ((both r s) (both (map-tags f r) (map-tags f s)))
    ((join r s) (join (map-tags f r) (map-tags f s)))
    ((kleene r) (kleene (map-tags f r)))
    ((invert r) (invert (map-tags f r)))
    ((alpha r old-tags)
     (unless (or *allow-alpha* (eq old-tags (empty-set)))
       (error "Can't modify tags with history"))
     (alpha (map-tags f r)
            (map-tags f old-tags)))
    ((grep r s)
     (grep (map-tags f r)
           (map-tags f s)))
    (_ re)))

(defun unique-tags (re)
  (let ((*allow-alpha* nil))
    (map-tags #'unique-assignments re)))
