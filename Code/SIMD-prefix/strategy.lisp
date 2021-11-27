(in-package :one-more-re-nightmare)

(defclass simd-prefix (strategy)
  ()
  (:documentation "Match a prefix of the string using SIMD operations before entering a DFA."))

(defmethod initial-states ((strategy simd-prefix) expression)
  ;; This includes the suffix produced by PREFIX, as PREFIX produces
  ;; some derivative of EXPRESSION.
  (list (add-tags expression)))

(defmethod start-code ((strategy simd-prefix) states)
  ;; Note that, by definition, having a prefix implies that the RE
  ;; can't produce only zero length matches, and it cannot match
  ;; nothing. Thus we don't have to worry about those cases.
  (multiple-value-bind (prefix suffix)
      (prefix (state-expression (first states)))
    (multiple-value-bind (test loads assignments jump-length)
        (code-from-prefix prefix)
      `(start
        ;; Don't try to read over the END we were given.
        (when (>= (the fixnum (+ ,jump-length
                                 one-more-re-nightmare.vector-primops:+v-length+
                                 position))
                  end)
          (go ,(find-state-name (first states) :bounds-check)))
        ;; Now perform the SIMD test.
        (let* (,@loads
               (test-results ,test))
          (unless (zerop test-results)
            ;; Found a match!
            )
          ;; No match, so just bump and try again.
          (incf position one-more-re-nightmare.vector-primops:+v-length+)
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
