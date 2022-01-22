(in-package :one-more-re-nightmare)

(defstruct transition
  class
  next-state
  tags-to-set)

(defclass state ()
  ((exit-map :initarg :exit-map :accessor state-exit-map)
   (exit-effects :initarg :exit-effects :accessor state-exit-effects)
   (expression :initarg :expression :accessor state-expression)
   (transitions :initform '() :accessor state-transitions)))

(defmethod print-object ((state state) stream)
  (print-unreadable-object (state stream :type t)
    (prin1 (state-expression state) stream)))

(defun find-similar-state (states state)
  "Find another state which we can re-use with some transformation, returning that state and the required transformation."
  (flet ((win (other-state substitutions)
           (return-from find-similar-state
             (values other-state
                     (loop with used = (used-tags (state-expression other-state))
                           for ((v1 r1) . (v2 r2))
                             in (alexandria:hash-table-alist substitutions)
                           when (member (list v2 r2) used :test #'equal)
                             collect (cons (list v2 r2) (list v1 r1)))))))
    (loop with expression = (state-expression state)
          for other-state in states
          for other-expression = (state-expression other-state)
          for substitutions = (similar expression other-expression)
          for used = (used-tags other-expression)
          unless (null substitutions)
            do (win other-state substitutions))))

(defun add-transition (class last-state next-state tags-to-set)
  (let* ((old-transitions (state-transitions last-state))
         (same-transition
           (find-if (lambda (transition)
                      (and
                       (equal tags-to-set (transition-tags-to-set transition))
                       (eq next-state (transition-next-state transition))))
                    old-transitions)))
    (cond
      ((null same-transition)
       (push (make-transition
              :class class
              :next-state next-state
              :tags-to-set tags-to-set)
             (state-transitions last-state)))
      (t
       (setf (transition-class same-transition)
             (set-union (transition-class same-transition)
                        class))))))

(trivia:defun-match re-stopped-p (re)
  ((alpha (empty-set) _) t)
  ((empty-set) t)
  (_ nil))

(defun peephole-optimize (assignments used-tags)
  (let ((result '())
        (remaining-assignments assignments))
    (flet ((substitute-variable (variable replica source)
             (setf remaining-assignments
                   (loop for ((v r) . s) in remaining-assignments
                         ;; Rewrite {A <- B} C <- A to C <- B
                         if (equal s (list variable replica))
                           collect (cons (list v r) source)
                         else
                           collect (cons (list v r) s)))))
      (loop until (null remaining-assignments)
            do (destructuring-bind ((variable replica) . source)
                   (pop remaining-assignments)
                 (if (member (list variable replica) used-tags :test #'equal)
                     (push (cons (list variable replica) source) result)
                     (substitute-variable variable replica source))))
      (reverse result))))
            

(defun make-dfa-from-expressions (expressions)
  (let ((states (make-hash-table))
        (possibly-similar-states (make-hash-table))
        (work-list expressions))
    (flet ((find-state (expression)
             (multiple-value-bind (state present?)
                 (gethash expression states)
               (if present?
                   (values state nil)
                   (values (setf (gethash expression states)
                                 (make-instance 'state
                                                :expression expression))
                           t)))))
      (loop
        (when (null work-list) (return))
        (let* ((expression (pop work-list))
               (state (find-state expression)))
          (cond
            ((or (re-stopped-p expression) (re-empty-p expression))
             nil)
            (t
             (let ((classes (derivative-classes expression)))
               (dolist (class classes)
                 (unless (set-null class)
                   (let* ((next-expression (derivative expression class))
                          (tags-to-set (keep-used-assignments
                                        next-expression
                                        (effects expression))))
                     (multiple-value-bind (next-state new?)
                         (find-state next-expression)
                       (multiple-value-bind (other-state transformation)
                           (find-similar-state
                            (cons state
                                  (gethash (remove-tags next-expression) possibly-similar-states '()))
                            next-state)
                         (cond
                           ((null other-state)
                            ;; No state to reuse, so check if we need to process the next state.
                            (when new?
                              (pushnew next-expression work-list)))
                           (t
                            ;; Reuse this state.
                            (when new?
                              (remhash (state-expression next-state) states))
                            (setf tags-to-set (peephole-optimize
                                               (append tags-to-set transformation)
                                               (used-tags (state-expression other-state)))
                                  next-state  other-state)))
                         (add-transition class
                                         state next-state
                                         tags-to-set)))))))))
          (push state (gethash (remove-tags expression)
                               possibly-similar-states))
          (setf (state-exit-map state)
                (tags (nullable expression))
                (state-exit-effects state)
                (keep-used-assignments
                 (nullable expression)
                 (effects expression))))))
    states))

(defun make-dfa-from-expression (expression)
  (make-dfa-from-expressions (list expression)))
