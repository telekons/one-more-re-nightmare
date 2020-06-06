(in-package :one-more-re-nightmare)

(define-type (literal set))

(defun empty-set ()
  (literal (symbol-set)))
(trivia:defpattern empty-set ()
  `(trivia:guard (literal set)
                 (set-null set)))
(define-type (empty-string)) ;; ε
(define-type (join r s)
             :simplify (((join (empty-set) _) (empty-set))
                        ((join _ (empty-set)) (empty-set))
                        ((join (empty-string) r) r)
                        ((join r (empty-string)) r))
             :hash-cons (((join (join r s) t) (join r (join s t)))))
(define-type (kleene r)
             :simplify (((kleene (kleene r)) (kleene r))))
(define-type (either r s)
             :simplify (((either r s)
                         (if (eq r s)
                             r
                             (trivia.next:next)))
                        ((either (empty-set) r) r)
                        ((either r (empty-set)) r)
                        ((either (literal s1) (literal s2))
                         (literal (set-union s1 s2))))
             :hash-cons (((either (either r s) t) (either r (either s t)))
                         ((either r s) (either s r))))
(define-type (both r s)
             :simplify (((both r s)
                         (if (eq r s)
                             r
                             (trivia.next:next))))
             :hash-cons (((both r s) (both s r))))
(define-type (invert r)
             :simplify (((invert (invert r)) r)
                        ((invert (empty-string)) (empty-set))))
