(in-package :one-more-re-nightmare)

(defstruct node
  (label (gensym))
  (code (alexandria:required-argument :code)))
(defstruct compiler-state
  (table (make-hash-table :test 'eq)))

(defun compile-regular-expression (regular-expression &key (vector-type 'vector))
  "Compile a function that will match the regular expression to a vector of type VECTOR-TYPE."
  (when (stringp regular-expression)
    (setf regular-expression (parse-regular-expression regular-expression)))
  (values (compile nil (make-lambda-form regular-expression
                                         :vector-type vector-type))))

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
      `(progn
         (go ,(label-for-expression compiler-state (empty-string))))
      `(progn
         (incf position)
         (go ,(label-for-expression compiler-state next-state)))))

(defun compile-expression-into-state (state regular-expression)
  "Add code for a regular expression to the compiler state."
  (let ((classes (derivative-classes regular-expression))
        (node (add-code state regular-expression nil)))
    (setf (node-code node)
          `(with-next-value
             (value (cond
                      ,@(loop for class in classes
                              for derivative = (derivative regular-expression class)
                              unless (set-null class)
                                collect `(,(make-test-form class 'value)
                                          ,(go-to-state-form state regular-expression derivative)))))
             ,(go-to-state-form state regular-expression (empty-set))))))

(defun make-lambda-form (regular-expression &key (vector-type 'vector))
  "Make a LAMBDA form that can be compiled to a function that matches the regular expression to vectors of VECTOR-TYPE."
  (let ((compiler-state (make-compiler-state)))
    (add-code compiler-state (empty-string)
              '(progn
                (funcall continuation this-start position)
                (if (= this-start position)
                    (setf this-start (1+ position))
                    (setf this-start position))
                (go loop)))
    (add-code compiler-state (empty-set)
              '(progn
                (incf this-start)
                (go loop)))
    `(lambda (vector start end continuation)
       (declare (optimize (speed 3) (safety 1)
                          (debug 0) (space 0)
                          (compilation-speed 0))
                (function continuation)
                (,vector-type vector)
                ((and fixnum (integer 0 *)) start end)
                (ignorable start end vector)
                #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
       (macrolet ((with-next-value ((value succeed-body)
                                    fail-body)
                    `(if (>= position end)
                         ,fail-body
                         (let ((,value (aref vector position)))
                           (declare (ignorable ,value))
                           ,succeed-body))))
         (prog* ((this-start start)
                 (position this-start))
            (declare (fixnum position this-start))
          loop
            (when (= this-start end)
              ,(if (nullable regular-expression)
                   '(funcall continuation this-start end)
                   nil)
              (return))
            (setf position this-start)
            (go ,(label-for-expression compiler-state regular-expression))
            ,@(loop for node being the hash-values of (compiler-state-table compiler-state)
                    appending `(,(node-label node)
                                ,(node-code node))))))))
