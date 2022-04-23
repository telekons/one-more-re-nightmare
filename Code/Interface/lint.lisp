(in-package :one-more-re-nightmare)

(define-condition lint-style-warning (style-warning)
  ())

(define-condition not-matchable-style-warning (lint-style-warning)
  ())

(define-condition expression-not-matchable (not-matchable-style-warning)
  ()
  (:report "This expression is impossible to match."))

(define-condition group-not-matchable (not-matchable-style-warning)
  ((n :initarg :n :reader warning-group-number)
   (string :initarg :string :reader warning-group-string))
  (:report
   (lambda (c s)
     (format s "The ~:R group~%~4T~A~%in this expression is impossible to match."
             (warning-group-number c)
             (warning-group-string c)))))

(define-condition matching-too-much-style-warning (lint-style-warning)
  ())

(define-condition expression-matches-everything (matching-too-much-style-warning)
  ()
  (:report "This expression can match the empty string at every position."))

(define-condition expression-matches-empty-string (matching-too-much-style-warning)
  ()
  (:report "This expression will only ever match the empty string at every position."))

(defun check-liveness (dfa groups group-strings)
  (let ((matching? nil)
        (matched-groups '()))
    (maphash (lambda (re state)
               (unless (eq (nullable re) (empty-set))
                 (setf matching? t))
               (loop for ((n nil) . nil) in (state-exit-map state)
                     ;; Group #1 uses tags #1 and #2 - we'll use
                     ;; #2 being written to test if #1 is alive.
                     for group-number = (floor (1+ n) 2)
                     when (evenp n)
                       do (pushnew group-number matched-groups)))
             dfa)
    (if (not matching?)
        (warn 'expression-not-matchable)
        (loop for n from 1 to groups
              unless (member n matched-groups)
                do (warn 'group-not-matchable
                         :n n
                         :string (aref group-strings n))))))

(defun check-empty-matches (dfa)
  (let ((warned-empty-string? nil)
        (expressions (remove (empty-set)
                             (alexandria:hash-table-alist dfa)
                             :key #'car)))
    ;; Check if this RE only matches the empty string. We can't just
    ;; do this syntactically, i.e. (eq expression (empty-string)) as e.g.
    ;; "|(a&bc)" confounds rewriting, and would produce a false negative.
    ;; Instead, we check that there is only one state that isn't the
    ;; empty set, and that it can only transition to the empty set.
    ;; Hence the DFA can only ever match the empty string.
    (when (alexandria:length= 1 expressions)
      (let ((re (car (first expressions))))
        (when (and (not (eq (nullable re) (empty-set)))
                   (null
                    (remove (empty-set)
                            (state-transitions (cdr (first expressions)))
                            :key (alexandria:compose #'state-expression
                                                     #'transition-next-state))))
          (warn 'expression-matches-empty-string)
          (setf warned-empty-string? t))))
    (unless (or warned-empty-string? (null expressions))
      ;; We will match everywhere if every state is nullable.
      (let ((something-wont-match? nil))
        (maphash (lambda (re state)
                   (declare (ignore state))
                   (when (and (eq (nullable re) (empty-set))
                              (not (eq re (empty-set))))
                     (setf something-wont-match? t)))
                 dfa)
        (unless something-wont-match? (warn 'expression-matches-everything))))))

(defun lint-regular-expression (expression)
  (with-hash-consing-tables ()
    (multiple-value-bind (expression group-count group-strings)
        (parse-regular-expression expression)
      (let ((dfa (make-dfa-from-expression expression)))
        (check-liveness dfa group-count group-strings)
        (check-empty-matches dfa)))))
