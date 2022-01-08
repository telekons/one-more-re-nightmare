(in-package :one-more-re-nightmare)

(defclass simd-info ()
  ((bits :initarg :bits :reader bits)))

(defclass simd-prefix (simd-info strategy)
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
                                 ,(/ one-more-re-nightmare.vector-primops:+v-length+ *bits*)
                                 start))
                  end)
          (setf position start)
          (go ,(find-state-name (first states) :bounds-check)))
        ;; Now perform the SIMD test.
        (let* (,@loads
               (test-results (,(find-op "MOVEMASK") ,test)))
          (unless (zerop test-results)
            ;; Found a match!
            (setf position (+ start (one-more-re-nightmare.vector-primops:find-first-set test-results)))
            ;; We increment POSITION by 1 before assigning to act like
            ;; the actual DFA, and then do the rest of the "jump"
            ;; after assignments.
            (incf position)
            ,assignments
            (incf position ,(1- jump-length))
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
                        (let ((position (1+ position)))
                          ,(setf-from-assignments effects))
                        (setf start (max (1+ start)
                                         (1- ,(find-in-map 'end (state-exit-map (second states))))))
                        (win ,@(win-locations
                                (loop for (variable replica nil) in effects
                                      collect (list variable replica))))))
                   `(go ,(find-state-name (second states) :bounds-check)))))
          ;; No match, so just bump and try again.
          (incf start ,(/ one-more-re-nightmare.vector-primops:+v-length+ *bits*))
          (go start))))))

(defmethod make-prog-parts :around ((strategy simd-info) expression)
  (let ((*broadcasts* (make-hash-table))
        (*bits* (bits strategy)))
    (multiple-value-bind (variables declarations body)
        (call-next-method)
      (maphash (lambda (value name)
                 (push (list name `(,(find-op "BROADCAST") ,value))
                       variables))
               *broadcasts*)
      (values variables declarations body))))

;; Surely we don't need this macro. Come on.
(defmethod macros-for-strategy append ((strategy simd-prefix))
  '((restart ()
     '(go start))))

(defun make-default-strategy (layout expression)
  (let ((bits
          (alexandria:switch ((layout-array-type layout) :test 'equal)
            ('(simple-array character 1) 32)
            ('(simple-array base-char 1) 8)
            (otherwise nil))))
    (cond
      ((and (> (count :literal (prefix expression) :key #'first) 1)
            (not (null bits)))
       (make-instance (dynamic-mixins:mix 'simd-loop 'simd-prefix 'call-continuation)
                      :bits bits))
      ((not (null bits))
       (make-instance (dynamic-mixins:mix 'simd-loop 'scan-everything 'call-continuation)
                      :bits bits))
      (t
       (make-instance (dynamic-mixins:mix 'scan-everything 'call-continuation))))))
