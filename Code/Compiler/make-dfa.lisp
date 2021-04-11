(in-package :one-more-re-nightmare)

(defstruct transition
  class
  next-state
  tags-to-set
  increment-position-p)

(defstruct state
  final-p
  exit-map)

(defun find-similar-state (states state)
  "Find another state which we can re-use with some transformation, returning that state and the required transformation."
  (loop for other-state being the hash-keys of states
        for substitutions = (similar state other-state)
        unless (null substitutions)
          do (return-from find-similar-state
               (values other-state
                       (loop for ((v1 r1) . source)
                               in (alexandria:hash-table-alist substitutions)
                             collect (list v1 r1 source))))))

(defun make-dfa-from-expressions (expressions)
  (let ((dfa    (make-hash-table))
        (states (make-hash-table))
        (work-list expressions))
    (setf (gethash (empty-set) dfa) '())
    (setf (gethash (empty-string) states)
          (make-state
           :final-p t
           :exit-map '()))
    (loop
      (when (null work-list) (return))
      (let* ((state  (pop work-list))
             (classes (derivative-classes state)))
        (cond
          ((re-empty-p state))
          (t
           (dolist (classes classes)
             (unless (set-null classes)
               (let* ((next-state (derivative state classes))
                      (tags-to-set (new-tags next-state state))
                      (increment-p t))
                 ;; Wire up this transition to "succeed" if it is nullable and
                 ;; it would usually fail.
                 (when (eq next-state (empty-set))
                   (let ((n (nullable state)))
                     (unless (eq n (empty-set))
                       (setf next-state n
                             tags-to-set (new-tags n state)
                             increment-p nil))))
                 (multiple-value-bind (other-state transformation)
                     (find-similar-state states next-state)
                   (cond
                     ((null other-state)
                      (unless (nth-value 1 (gethash next-state dfa))
                        (pushnew next-state work-list)))
                     (t                 ; Reuse this state.
                      (setf tags-to-set (merge-tag-sets transformation
                                                        tags-to-set)
                            next-state  other-state))))
                 (push (make-transition
                        :class classes
                        :next-state next-state
                        :tags-to-set tags-to-set
                        :increment-position-p increment-p)
                       (gethash state dfa)))))))
        (let ((n (nullable state)))
          (setf (gethash state states)
                (make-state :final-p (not (eq n (empty-set)))
                            :exit-map (tags n))))))
    (values dfa states)))

(defun make-dfa-from-expression (expression)
  (make-dfa-from-expressions (list expression)))

(defun print-dfa (dfa)
  (maphash (lambda (state transitions)
             (dolist (transition transitions)
               (format t "~&\"~s\"~&  -> \"~s\"[label=\"~s\"]"
                       state
                       (transition-next-state transition)
                       (transition-class transition))))
           dfa))
