(in-package :one-more-re-nightmare)

(define-hash-consing-table *derivative*)

(defun derivative (re set)
  "Compute the derivative of a regular expression with regards to the set (i.e. the regular expression should be matched after a character in the set is matched)."
  (with-hash-consing (*derivative* (list re set))
    (trivia:ematch re
      ((or (empty-string) (empty-set) (tag-set _)) (empty-set))
      ((literal matching-set)
       (if (set-null (set-intersection matching-set set))
           (empty-set)
           (empty-string)))
      ((join r s)
       (let ((r* (derivative r set))
             (s* (derivative s set)))
         (cond
           ((eq r* (empty-set))
            ;; Something like [...]A doesn't need gensym'ing.
            (let ((*gensym-assignments?* nil))
              (join (nullable r) s*)))
           ((not (has-tags-p r*))
            ;; Ditto for A[...]
            (either (join r* s) (join (nullable r) s*)))
           (t
            (either (join r* (unique-tags s)) (join (nullable r) s*))))))
      ((kleene r)
       (join (derivative r set) (kleene (unique-tags r))))
      ((either r s)
       (either (derivative r set) (derivative s set)))
      ((both r s)
       (both (derivative r set) (derivative s set)))
      ((invert r)
       (invert (derivative r set)))
      ((grep r s)
       (let* ((r* (derivative r set))
              (n (nullable r*)))
         (if (eq n (empty-set))
             (grep (either r*
                           (if (has-tags-p r*)
                               (unique-tags s)
                               s))
                   s)
             r*)))
      ((alpha r old-tags)
       (let* ((r* (derivative r set))
              (*gensym-assignments?* nil)
              (nullable (nullable r)))
         (alpha r*
                (either nullable old-tags)))))))

(defun derivative* (re sequence)
  (let ((variables (make-hash-table :test 'equal))
        (position 0))
    (flet ((run-effects (effects)
             (loop for (target . source) in effects
                   for value = (case source
                                 ((position) position)
                                 ((nil) 'nil)
                                 (otherwise (gethash source variables :unbound)))
                   do (setf (gethash target variables) value))))
      (map 'nil
           (lambda (element)
             (let* ((new-re (derivative re (symbol-set (char-code element))))
                    (effects (remove-if (lambda (x) (equal (car x) (cdr x)))
                                        (effects re))))
               (format t "~&~a~&  ~:c ~a"
                       re element effects)
               (setf re new-re)
               (run-effects effects)
               (incf position)))
           sequence)
      (run-effects (effects re))
      (values re
              (trivia:match (nullable re)
                ((tag-set s)
                 (loop for ((name nil) . source) in s
                       unless (null (gethash source variables))
                       collect (cons name (gethash source variables))))
                ((empty-string) '())
                ((empty-set) '()))
              (nullable re)))))
