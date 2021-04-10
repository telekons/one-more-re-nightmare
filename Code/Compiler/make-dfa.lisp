(in-package :one-more-re-nightmare)

(defstruct transition
  group
  next-state
  tags-to-set)

(defstruct state
  final-p
  exit-map)

(defun make-dfa-from-expressions (expressions)
  (let ((dfa    (make-hash-table))
        (states (make-hash-table))
        (work-list expressions))
    (setf (gethash (empty-set) dfa) '())
    (loop
      (when (null work-list) (return))
      (let* ((state  (pop work-list))
             (groups (derivative-classes state)))
        (cond
          ((re-empty-p state))
          (t
           (dolist (group groups)
             (unless (set-null group)
               (let ((next-state (derivative state group))
                     (tags-to-set '()))
                 (dolist (tag (new-tags next-state state))
                   (push tag tags-to-set))
                 (unless (nth-value 1 (gethash next-state dfa))
                   (pushnew next-state work-list))
                 (push (make-transition
                        :group group
                        :next-state next-state
                        :tags-to-set tags-to-set)
                       (gethash state dfa)))))))
        (let ((n (nullable state)))
          (setf (gethash state states)
                (make-state :final-p (not (eq n (empty-set)))
                            :exit-map n)))))
    (values dfa states)))

(defun make-dfa-from-expression (expression)
  (make-dfa-from-expressions (list expression)))

(defun print-dfa (dfa)
  (maphash (lambda (state transitions)
             (dolist (transition transitions)
               (format t "~&\"~s\" -> \"~s\"[label=\"~s\"]"
                       state
                       (transition-next-state transition)
                       (transition-group transition))))
           dfa))
