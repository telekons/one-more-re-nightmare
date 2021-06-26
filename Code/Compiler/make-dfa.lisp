(in-package :one-more-re-nightmare)

(defstruct transition
  class
  next-state
  tags-to-set
  increment-position-p)

(defstruct state
  final-p
  exit-map)

(defun find-similar-state (states old-state state)
  "Find another state which we can re-use with some transformation, returning that state and the required transformation."
  (flet ((win (other-state substitutions)
           (return-from find-similar-state
             (values other-state
                     (loop with used = (used-tags other-state)
                           for ((v1 r1) . source)
                             in (alexandria:hash-table-alist substitutions)
                           when (member (list v1 r1) used :test #'equal)
                             collect (list v1 r1 source))))))
    (let ((subs (similar state old-state)))
      (unless (null subs)
        (win old-state subs)))
    (loop for other-state being the hash-keys of states
          for substitutions = (similar state other-state)
          for used = (used-tags other-state)
          unless (null substitutions)
            do (win other-state substitutions))))

(defun add-transition (class last-state next-state tags-to-set increment-p dfa)
  (let* ((old-transitions (gethash last-state dfa))
         (same-transition
           (find-if (lambda (transition)
                      (and
                       (equal tags-to-set (transition-tags-to-set transition))
                       (eq next-state (transition-next-state transition))
                       (eq increment-p
                           (transition-increment-position-p transition))))
                    old-transitions)))
    (cond
      ((null same-transition)
       (push (make-transition
              :class class
              :next-state next-state
              :tags-to-set tags-to-set
              :increment-position-p increment-p)
             (gethash last-state dfa)))
      (t
       (setf (transition-class same-transition)
             (set-union (transition-class same-transition)
                        class))))))

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
          ((re-empty-p state) nil)
          (t
           (dolist (class classes)
             (unless (set-null class)
               (let* ((next-state (derivative state class))
                      (tags-to-set (new-tags next-state state))
                      (increment-p t))
                 (multiple-value-bind (other-state transformation)
                     (find-similar-state states state next-state)
                   (cond
                     ((null other-state)
                      (unless (nth-value 1 (gethash next-state dfa))
                        (pushnew next-state work-list)))
                     (t                 ; Reuse this state.
                      (setf tags-to-set (merge-tag-sets transformation
                                                        tags-to-set)
                            next-state  other-state))))
                 (add-transition class
                                 state next-state
                                 tags-to-set increment-p dfa))))))
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
               (format t "~&\"~s\"~&  -> \"~s\"[label=\"~s ~s\"]"
                       state
                       (transition-next-state transition)
                       (transition-class transition)
                       (tag-set
                        (transition-tags-to-set transition)))))
           dfa))
