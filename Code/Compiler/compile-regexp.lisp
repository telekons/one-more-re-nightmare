(in-package :one-more-re-nightmare)

(defstruct node
  (label (gensym))
  (code (alexandria:required-argument :code)))
(defstruct compiler-state
  (table (make-hash-table :test 'eq))
  (variable-table (make-hash-table :test 'eql)))

(defun compile-regular-expression (regular-expression
                                   &key (vector-type 'vector)
                                        (aref-generator (lambda (vector position)
                                                          `(aref ,vector ,position))))
  "Compile a function that will match the regular expression to a vector of type VECTOR-TYPE."
  (when (stringp regular-expression)
    (setf regular-expression (parse-regular-expression regular-expression)))
  (values (compile nil (make-lambda-form regular-expression
                                         :vector-type vector-type
                                         :aref-generator aref-generator))))

(defun add-code (compiler-state expression code)
  (setf (gethash expression (compiler-state-table compiler-state))
        (make-node :code code
                   :label (make-symbol
                           (with-standard-io-syntax
                             (princ-to-string expression))))))

(defun label-for-expression (state regular-expression)
  "Returns the label name for a regular expression."
  (multiple-value-bind (node present?)
      (gethash regular-expression (compiler-state-table state))
    (if present?
        (node-label node)
        (progn
          (compile-expression-into-state state regular-expression)
          (label-for-expression state regular-expression)))))

(defun go-to-state-form (compiler-state current-state next-state)
  (if (and (nullable current-state) (eq (empty-set) next-state))
      `(go ,(label-for-expression compiler-state (empty-string)))
      `(progn
         (incf position)
         (go ,(label-for-expression compiler-state next-state)))))

(defun group-variable (state n type)
  (multiple-value-bind (variables present?)
      (gethash n (compiler-state-variable-table state))
    (if (not present?)
        (progn
          (setf (gethash n (compiler-state-variable-table state))
                (list (alexandria:format-symbol nil "START-~d" n)
                      (alexandria:format-symbol nil "END-~d" n)
                      (alexandria:format-symbol nil "~d-VALID?" n)))
          (group-variable state n type))
        (destructuring-bind (start end valid?)
            variables
          (trivia:ematch type
            (:start start)
            (:end end)
            (:valid? valid?))))))

(defun group-bindings (state)
  (loop for (start end valid?) being the hash-values
          in (compiler-state-variable-table state)
        appending `((,start 0) (,end 0) (,valid? nil))))

(defun reset-group-bindings (state)
  `(setf
    ,@(loop for (start end valid?) being the hash-values
              in (compiler-state-variable-table state)
            appending `(,start 0 ,end 0 ,valid? nil))))

(defun groups-list (state)
  (if (zerop (hash-table-count (compiler-state-variable-table state)))
      ''()
      `(remove nil
               (list
                ,@(loop with table = (compiler-state-variable-table state)
                        for index in (sort (alexandria:hash-table-keys table)
                                           #'<)
                        for (start end valid?) = (gethash index table)
                        collect `(if ,valid?
                                     (list ,index ,start ,end)
                                     nil))))))

(defun generate-effect-code (state effect)
  (trivia:ematch effect
    ((start-group n)
     `(setf ,(group-variable state n :start)  position
            ,(group-variable state n :end)    position
            ,(group-variable state n :valid?) t))
    ((end-group n)
     `(setf ,(group-variable state n :end) position))))

(defun compile-expression-into-state (state regular-expression)
  "Add code for a regular expression to the compiler state."
  (let ((classes (derivative-classes regular-expression))
        (node (add-code state regular-expression nil)))
    (setf (node-code node)
          `(,@(loop for effect in (effects regular-expression)
                    collect (generate-effect-code state effect))
            (with-next-value
                (value (cond
                         ,@(loop for class in classes
                                 for derivative = (derivative regular-expression class)
                                 unless (set-null class)
                                   collect `(,(make-test-form class 'value)
                                             ,(go-to-state-form state regular-expression derivative)))))
              ,(go-to-state-form state regular-expression (empty-set)))))))

(defun make-lambda-form (regular-expression
                         &key (vector-type 'vector)
                           (aref-generator (lambda (vector position)
                                             `(aref ,vector ,position))))
  "Make a LAMBDA form that can be compiled to a function that matches the regular expression to vectors of VECTOR-TYPE."
  (multiple-value-bind (prefix suffix)
      (prefix regular-expression)
    (let ((compiler-state (make-compiler-state)))
      ;; We'll put real code in shortly, after we finish producing the DFA and
      ;; have submatch variable names to include.
      (add-code compiler-state (empty-string) '((error "bogus")))
      (add-code compiler-state (empty-set)
                `((incf this-start)
                  (go loop)))
      (compile-expression-into-state compiler-state
                                     suffix)
      (setf (node-code (gethash (empty-string)
                                (compiler-state-table compiler-state)))
            `((funcall continuation this-start position
                       ,(groups-list compiler-state))
              (if (= this-start position)
                  (incf this-start)
                  (setf this-start position))
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
                     (position this-start)
                     ,@(group-bindings compiler-state))
                (declare (fixnum position this-start))
              loop
                ,(boyer-moore-horspool-search-expression 'this-start 'end 'vector
                                                         prefix
                                                         aref-generator
                                                         `(progn
                                                            ,(if (nullable regular-expression)
                                                                 `(funcall continuation this-start end
                                                                           ,(groups-list compiler-state))
                                                                 (progn))
                                                            (return-from scan)))
                (setf position (+ this-start ,(length prefix)))
                ,(reset-group-bindings compiler-state)
                (go ,(label-for-expression compiler-state suffix))
                ,@(loop for node being the hash-values of (compiler-state-table compiler-state)
                        appending `(,(node-label node)
                                    ,@(node-code node))))))))))
