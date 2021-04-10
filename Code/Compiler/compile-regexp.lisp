(in-package :one-more-re-nightmare)

(defstruct tagbody-state
  name code)

(defstruct compiler-state
  (table (make-hash-table :test 'eq))
  (variable-table (make-hash-table :test 'equal)))

(defun tag-variable-name (state name)
  (or (gethash name (compiler-state-variable-table state))
      (setf (gethash name (compiler-state-variable-table state))
            (alexandria:format-symbol nil "~a" name))))

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
        appending (cons (tagbody-state-name tagbody)
                        (tagbody-state-code tagbody))))

(defun make-lambda-form (dfa states
                         initial-state
                         vector-type aref-generator)
  "Make a LAMBDA form that can be compiled to a function that matches the regular expression to vectors of VECTOR-TYPE."
    (let ((compiler-state (make-compiler-state)))
      ;; We'll put real code in shortly, after we finish producing the DFA and
      ;; have submatch variable names to include.
      (add-code compiler-state (empty-string)
                `((funcall continuation this-start position tags)
                  (if (= this-start position)
                      (incf this-start)
                      (setf this-start position))
                  (go loop)))
      (add-code compiler-state (empty-set)
                `((incf this-start)
                  (go loop)))
      `(lambda (vector start end continuation)
         (declare (optimize (speed 3) (safety 0)
                            (debug 0) (space 0)
                            (compilation-speed 0))
                  (function continuation)
                  (,vector-type vector)
                  (alexandria:array-length start end)
                  (ignorable start end vector)
                  #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
         (block scan
           (macrolet ((with-next-value ((value succeed-body)
                                        fail-body)
                        `(if (>= position end)
                             ,fail-body
                             (let ((,value ,',(funcall aref-generator
                                                       'vector 'position)))
                               (declare (ignorable ,value))
                               ,succeed-body))))
             (prog* ((this-start start)
                     (position this-start))
                (declare (fixnum position this-start))
              loop
                (go ,(re-name compiler-state initial-state))
              ,@(state-body compiler-state)))))))
