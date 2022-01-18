(in-package :one-more-re-nightmare)

(defun find-live-registers (dfa)
  (let ((live (make-hash-table :test 'equal))
        (sources (make-hash-table :test 'equal))
        (work-list '()))
    (loop for state being the hash-values of dfa
          ;; We first walk the exit maps to find the first live registers,
          do (loop for (nil . source) in (state-exit-map state)
                   do (trivia:match source
                        ((list _ _)
                         (pushnew source work-list :test #'equal))))
             ;; and we walk the transitions to get an assignment list,
             ;; which is later used to quickly find sources of
             ;; registers.
             (flet ((scan (assignments)
                      (loop for (target . source) in assignments
                            do (pushnew source (gethash target sources) :test #'equal))))
               (scan (state-exit-effects state))
               (dolist (trans (state-transitions state))
                 (scan (transition-tags-to-set trans)))))
    ;; We then walk the transitive closure to find which other registers are live.
    (loop
      (when (null work-list)
        (return))
      (let ((register (pop work-list)))
        (unless (gethash register live) ; checked already?
          (setf (gethash register live) t)
          (dolist (source (gethash register sources))
            (pushnew source work-list :test #'equal)))))
    live))

(defun remove-dead-writes (dfa)
  "Remove writes to registers which will never be used to match a RE."
  (let ((live (find-live-registers dfa)))
    (macrolet ((update (place)
                 `(setf ,place
                        (loop for assignment in ,place
                              for (target . nil) = assignment
                              when (gethash target live)
                                collect assignment))))
      (loop for state being the hash-values of dfa
            do (update (state-exit-effects state))
               (dolist (trans (state-transitions state))
                 (update (transition-tags-to-set trans)))))))
