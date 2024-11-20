(in-package :one-more-re-nightmare)

(define-types
  ((literal set))
  ((empty-string))
  ((repeat r min max can-empty))
  ((tag-set substitutions) equal tag-set-hash)
  ((alpha expression history))
  ((grep vector prototype))
  ((either r s))
  ((both r s))
  ((invert r))
  ((join r s)))

(define-rewrites (literal set)
  :printer ((literal set)
            (print-csum set stream)))

(defun kleene (r)
  (repeat r 0 nil nil))
(trivia:defpattern kleene (r)
  `(repeat ,r 0 nil nil))

(defun empty-set () (literal +empty-set+))
(trivia:defpattern empty-set ()
  (alexandria:with-gensyms (set)
    `(trivia:guard (literal ,set)
                   (csum-null-p ,set))))

(defun universal-set ()
  (repeat (literal +universal-set+) 0 nil nil))
(trivia:defpattern universal-set ()
  `(kleene (literal ',+universal-set+)))

(defvar *subscripts* "₀₁₂₃₄₅₆₇₈₉")
(defun subscripts (number)
  (map 'string
       (lambda (char)
         (aref *subscripts* (digit-char-p char)))
       (princ-to-string number)))

(define-rewrites (empty-string)
  :printer (_ (write-string "ε" stream)))

(define-rewrites (repeat r min max can-empty)
  :simplify (((repeat _ _ 0 _) (empty-string))
             ((repeat r 1 1 _) r)
             ((repeat (empty-set) 0 nil _) (empty-string))
             ((repeat (repeat r 0 nil _) 0 nil c)
              (repeat r 0 nil c)))
  :printer ((repeat r min max can-empty)
            (format stream "[~a]{~a~a,~a}"
                    r (if can-empty "^" "")
                    min
                    (or max ""))))

(define-rewrites (tag-set substitutions)
  :simplify (((tag-set (list))
              (empty-string)))
  :printer ((tag-set s)
            (format stream "{~{~a ← ~a~^, ~}}"
                    (loop for ((variable replica) . source) in s
                          if (zerop replica)
                            collect variable
                          else
                            collect (format nil "~a~a"
                                            variable
                                            (subscripts replica))
                          collect (case source
                                    ((position) "P")
                                    ((nil) "NIL")
                                    (otherwise
                                     (format nil "~a~a"
                                             (first source)
                                             (subscripts (second source)))))))))
(define-rewrites (alpha expression history)
  :simplify (((alpha (empty-set) (empty-set)) (empty-set)))
  :printer ((alpha r n)
            (format stream "α[~@<~a, ~_~a]~:>" r n)))
(define-rewrites (grep match-vector prototype)
  :simplify (((grep r _)
              (if (eq (nullable r) (empty-set))
                  (trivia.next:next)
                  r))
             ((grep (empty-set) _)
              (empty-set)))
  :printer ((grep r _)
            (format stream "γ[~a]" r)))

(define-rewrites (either r s)
  :simplify (((either (either p r) s)
              (either p (either r s)))
             ((either (empty-set) r) r)
             ((either r (empty-set)) r)
             ((either (literal s1) (literal s2))
              (literal (csum-union s1 s2)))
             ((either r (universal-set))
              (if (has-tags-p r)
                  (trivia.next:next)    ; Preserve tags then
                  (universal-set)))
             ((either (join (literal s1) p)
                      (join (literal s2) r))
              ;; Try to expose more prefixes.
              (if (equal s1 s2)
                  (join (literal s1) (either p r))
                  (trivia.next:next)))
             ((either r s)
              (scan-either-for-duplicates r s)))
  :printer ((either r s)
            (format stream "~@<(~a) ∪ ~_(~a)~:>" r s)))

(defun scan-either-for-duplicates (r s)
  (labels ((e (r s)
             ;; We can't call EITHER, since EITHER calls us, so we
             ;; handle simplifying out empty sets here.
             (cond
               ((eq s (empty-set)) r)
               ((eq r (empty-set)) s)
               (t (%either r s))))
           (scan (rhs)
             (trivia:match rhs
               ((either lhs next-rhs)
                (if (eq (remove-tags lhs) (remove-tags r))
                    next-rhs
                    (e lhs (scan next-rhs))))
               (_
                (if (eq (remove-tags rhs) (remove-tags r))
                    (empty-set)
                    rhs)))))
    (e r (scan s))))

(define-rewrites (both r s)
  :simplify (((both r s)
              (if (eq r s)
                  r
                  (trivia.next:next)))
             ((both _ (empty-set)) (empty-set))
             ((both (empty-set) _) (empty-set))
             ((both (tag-set s1) (tag-set s2))
              (tag-set (merge-tag-sets s1 s2)))
             ((both (tag-set s) (empty-string))
              (tag-set s))
             ((both (empty-string) (tag-set s))
              (tag-set s))
             ((both (literal s1) (literal s2))
              (literal (csum-intersection s1 s2))))
  :printer ((both r s)
            (format stream "(~a) ∩ (~a)" r s)))
(define-rewrites (invert r)
  :simplify (((invert (invert r)) r)
             ((invert s)
              (if (has-tags-p s)
                  (invert (remove-tags s))
                  (trivia.next:next)))
             ((invert (universal-set)) (empty-set)))
  :printer ((invert r)
            (format stream "¬[~a]" r)))
(define-rewrites (join r s)
  :simplify (((join (tag-set s) (either p r))
              (either (join (tag-set s) p)
                      (join (tag-set s) r)))
             ((join (empty-set) _) (empty-set))
             ((join _ (empty-set)) (empty-set))
             ((join (empty-string) r) r)
             ((join r (empty-string)) r)
             ((join (join (tag-set s1) r) s)
              ;; Rotate so that a TAG-SET is always at the start.
              (join (tag-set s1) (join r s)))
             ((join (tag-set s1) (join (tag-set s2) r))
              (join (tag-set (merge-tag-sets s1 s2)) r))
             ;; Avoid A*A*
             ((join (kleene a) (join (kleene b) c))
              (if (eq a b)
                  (join (kleene a) c)
                  (trivia.next:next)))
             ((join (kleene a) (kleene b))
              (if (eq a b)
                  (kleene a)
                  (trivia.next:next)))
             ((join (tag-set s1) (tag-set s2))
              (tag-set (merge-tag-sets s1 s2))))
  :printer ((join r s)
            (format stream "~a~a" r s)))

(defun text (vector)
  (reduce #'join (map 'vector
                      (lambda (e)
                        (literal (singleton-set (char-code e))))
                      vector)
          :initial-value (empty-string) 
          :from-end t))

(defun group (r n)
  (join (tag-set `(((,(1- (* 2 n)) 0) . position)))
        (join r
              (tag-set `(((,(* 2 n) 0) . position))))))
