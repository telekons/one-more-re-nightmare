(in-package :one-more-re-nightmare)

(defclass simd-loop (simd-info) ())

(defun assignments-idempotent-p (assignments)
  "Are the assignments idempotent, i.e. would repeated applications of the assignments, interleaved with incrementing the position, be the same as applying the assignments once at the end?"
  ;; TODO: Be much more clever about this. We can't form un-idempotent
  ;; assignments unless we write a variable we also read.
  (null assignments))

(defmethod transition-code ((strategy simd-loop) previous-state transition)
  (let* ((next-state (transition-next-state transition))
         (next-expression (state-expression next-state)))
    ;; We optimise tight loops like A -> A.
    (when (or (re-stopped-p next-expression)
              (re-empty-p next-expression)
              (not (eq next-state previous-state))
              (not (assignments-idempotent-p
                    (transition-tags-to-set transition))))
      (return-from transition-code (call-next-method)))
    ;; Try to skip to the first character after for which this transition doesn't apply.
    (let* ((vector-length (/ one-more-re-nightmare.vector-primops:+v-length+ *bits*)))
      (trivia:ematch
          (test-from-isum 'loaded
                          (set-inverse (transition-class transition)))
        (:never (call-next-method))
        (:always (error "Found a transition that is never taken."))
        (test
         `(progn
            ;; Don't try to read over the END we were given.
            (loop
              (unless (< (the fixnum (+ ,vector-length position)) end)
                (return))
              (let* ((loaded (,(find-op "LOAD") vector position))
                     (test   (,(find-op "MOVEMASK") ,test)))
                (unless (zerop test)
                  (incf position (one-more-re-nightmare.vector-primops:find-first-set test))
                  (return)))
              (incf position ,vector-length))
            ,(call-next-method)))))))
