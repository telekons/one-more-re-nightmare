(in-package :one-more-re-nightmare)

(define-condition expression-not-matchable (style-warning)
  ()
  (:report "This expression is impossible to match."))

(define-condition group-not-matchable (style-warning)
  ((n :initarg :n :reader warning-group-number))
  (:report
   (lambda (c s)
     (format s "The ~:r group in this expression is impossible to match."
             (warning-group-number c)))))

(defun lint-regular-expression (expression)
  (with-hash-consing-tables ()
    (multiple-value-bind (expression groups)
        (parse-regular-expression expression)
      (let ((dfa (make-dfa-from-expression expression))
            (matching? nil)
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
        (cond
          ((not matching?)
           (warn 'expression-not-matchable))
          (t
           (loop for n from 1 to groups
                 unless (member n matched-groups)
                   do (warn 'group-not-matchable :n n))))))))
