(in-package :one-more-re-nightmare)

(defclass simd-prefix (strategy)
  ()
  (:documentation "Match a prefix of the string using SIMD operations before entering a DFA.
A prefix P of some regular expression R is defined to be a sequence of literals such that P·S = R for some other suffix regular expression S."))

(defmethod initial-states ((strategy simd-prefix) expression)
  (multiple-value-bind (prefix suffix)
      (prefix (add-tags expression))
    (declare (ignore prefix))
    (list (alpha (add-tags expression) (empty-set))
          (alpha suffix (empty-set)))))

(defmethod start-code ((strategy simd-prefix) states)
  ;; Note that, by definition, having a prefix implies that the RE
  ;; can't produce only zero length matches, and it cannot match
  ;; nothing. Thus we don't have to worry about those cases.
  (multiple-value-bind (prefix suffix)
      (prefix (state-expression (first states)))
    (declare (ignore suffix))
    (multiple-value-bind (test loads assignments jump-length)
        (code-from-prefix prefix)
      `(start
        ;; Don't try to read over the END we were given.
        (when (>= (the fixnum (+ ,jump-length
                                 one-more-re-nightmare.vector-primops:+v-length+
                                 start))
                  end)
          (go ,(find-state-name (first states) :bounds-check)))
        ;; Now perform the SIMD test.
        (let* (,@loads
               (test-results ,test))
          (unless (zerop test-results)
            ;; Found a match!
            (setf position (+ start (one-more-re-nightmare.vector-primops:find-first-set test-results)))
            ,assignments
            (incf position ,jump-length)
            ;; The same deal as in START-CODE for SCAN-EVERYTHING: we
            ;; "inline" succeeding states, so we might need to succeed
            ;; rather than go to another state.
            ,(let ((expression (state-expression (second states))))
               (if (re-empty-p expression)
                   (let ((effects (effects expression)))
                     `(progn
                        ;; Surely there wouldn't be any new
                        ;; assignments, as PREFIX would strip them
                        ;; off.
                        ,@(setf-from-assignments effects)
                        (win ,@(win-locations
                                (loop for (variable replica nil) in effects
                                      collect (list variable replica))))))
                   `(go ,(find-state-name (second states) :bounds-check)))))
          ;; No match, so just bump and try again.
          (incf start one-more-re-nightmare.vector-primops:+v-length+)
          (go start))))))

(defmethod make-prog-parts :around ((strategy simd-prefix) expression)
  (let ((*broadcasts* (make-hash-table)))
    (multiple-value-bind (variables declarations body)
        (call-next-method)
      (maphash (lambda (value name)
                 (push (list name `(one-more-re-nightmare.vector-primops:v-broadcast ,value))
                       variables))
               *broadcasts*)
      (values variables declarations body))))
