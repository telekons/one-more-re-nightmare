(in-package :one-more-re-nightmare)

(defun compile-regular-expression (regular-expression
                                   &key (vector-type 'vector)
                                        (aref-generator (lambda (vector position)
                                                          `(aref ,vector ,position))))
  "Compile a function that will match the regular expression to a vector of type VECTOR-TYPE."
  (when (stringp regular-expression)
    (setf regular-expression (parse-regular-expression regular-expression)))
  (multiple-value-bind (dfa states)
      (make-dfa-from-expression regular-expression)
    (values (compile nil (make-lambda-form dfa states
                                           (map 'vector #'first
                                                (tags regular-expression))
                                           regular-expression
                                           vector-type
                                           aref-generator)))))

(defstruct tagbody-state
  name code)

(defstruct compiler-state
  (table (make-hash-table :test 'eq))
  (variable-table (make-hash-table :test 'equal)))

(defun tag-variable-name (state name)
  (trivia:ematch name
    ('position 'position)
    ((list _ _)
     (or (gethash name (compiler-state-variable-table state))
         (setf (gethash name (compiler-state-variable-table state))
               (alexandria:format-symbol nil "~a" name))))))

(defun tag-variable-names (state)
  (loop for name being the hash-values
          of (compiler-state-variable-table state)
        collect name))

(defun re-state (state re)
  (or (gethash re (compiler-state-table state))
      (setf (gethash re (compiler-state-table state))
            (make-tagbody-state
             :name (alexandria:format-symbol nil "~a" re)))))

(defun re-name (state re)
  (tagbody-state-name (re-state state re)))

(defun add-code (state re code)
  (setf (tagbody-state-code (re-state state re)) code))

(defun state-body (state)
  (loop for tagbody being the hash-values of (compiler-state-table state)
        do (assert (not (null (tagbody-state-code tagbody))))
        appending (list (tagbody-state-name tagbody)
                        (tagbody-state-code tagbody))))

(defun generate-dispatch-code (compiler-state transitions)
  `(cond
     ,@(loop for transition in transitions
             collect `(,(make-test-form (transition-class transition) 'value)
                       (setf ,@(loop for (variable replica source)
                                       in (transition-tags-to-set transition)
                                     appending (list (tag-variable-name compiler-state
                                                                        (list variable replica))
                                                     (tag-variable-name compiler-state source))))
                       (incf position)
                       (go ,(re-name compiler-state (transition-next-state transition)))))))

(defun generate-tags-code (compiler-state re state tag-names)
  (let ((sources (make-array (length tag-names)
                             :initial-element ''nil)))
    ;; Find forms to populate each element of the tag vector with.
    (loop for (variable replica source) in (state-exit-map state)
          for position = (position variable tag-names)
          do (assert (not (null position)))
             (setf (aref sources position)
                   (tag-variable-name compiler-state source)))
    `(progn
       ,@(loop for (variable replica source) in (tags re)
               collect `(setf ,(tag-variable-name compiler-state
                                                  (list variable replica))
                              ,(tag-variable-name compiler-state source)))
       ,@(loop for source across sources
               for position from 0
               collect `(setf (svref tags ,position) ,source)))))

(defun generate-code-for-state (compiler-state re state dfa tag-names)
  "Generate code to be used to step through a state in the DFA."
  (if (re-empty-p re)
      ;; Report success and keep scanning.
      `(progn
         ,(generate-tags-code compiler-state re state tag-names)
         (funcall continuation this-start position)
         (cond
           ((= this-start position)
            (incf this-start)
            (go loop))
           (t
            (setf this-start position)
            (go loop))))
      `(with-next-value value
         ;; If we have a value, then dispatch to the next state.
         ,(generate-dispatch-code compiler-state (gethash re dfa))
         ,(if (state-final-p state)
              ;; If we EOF on an accepting state, we still won.
              `(progn
                 ,(generate-tags-code compiler-state re state tag-names)
                 (funcall continuation this-start position)
                 (return-from scan))
              `(return-from scan)))))

(defun make-lambda-form (dfa states tag-names
                         initial-state vector-type aref-generator)
  "Make a LAMBDA form that can be compiled to a function that matches the regular expression to vectors of VECTOR-TYPE."
  (let ((compiler-state (make-compiler-state)))
      ;; We'll put real code in shortly, after we finish producing the DFA and
    ;; have submatch variable names to include.
    (let ((e (empty-string)))
      (add-code compiler-state e
                (generate-code-for-state compiler-state
                                         e (gethash e states)
                                         dfa tag-names)))
    (maphash (lambda (state information)
               (add-code compiler-state state
                         (generate-code-for-state compiler-state
                                                  state information
                                                  dfa tag-names)))
             states)
    (add-code compiler-state (empty-set)
              `(progn
                 (incf this-start)
                 (go loop)))
    `(lambda (vector start end tags continuation)
       (declare (optimize (speed 3) (safety 0)
                          (debug 0) (space 0)
                          (compilation-speed 0))
                (function continuation)
                (,vector-type vector)
                (simple-vector tags)
                (alexandria:array-length start end)
                (ignorable start end vector)
                #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
       (block scan
         (macrolet ((with-next-value (value succeed-body fail-body)
                      `(if (>= position end)
                           ,fail-body
                           (let ((,value
                                     ,',(funcall aref-generator 'vector 'position)))
                               (declare (ignorable ,value))
                               ,succeed-body))))
             (prog* ((this-start start)
                     (position this-start)
                     ,@(loop for name in (tag-variable-names compiler-state)
                             collect `(,name nil)))
                (declare (alexandria:array-index position this-start)
                         ((or alexandria:array-index null)
                          ,@(tag-variable-names compiler-state)))
              loop
                (setf position this-start)
                (go ,(re-name compiler-state initial-state))
              ,@(state-body compiler-state)))))))
