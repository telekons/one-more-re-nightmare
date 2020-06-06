(in-package :one-more-re-nightmare)

(defstruct node
  (label (gensym))
  (code (alexandria:required-argument :code)))
(defstruct compiler-state
  (table (make-hash-table :test 'eq)))

(defun compile-regular-expression (regular-expression &key (vector-type 'vector))
  (values (compile nil (make-lambda-form regular-expression
                                         :vector-type vector-type))))

(defun add-code (compiler-state expression code)
  (setf (gethash expression (compiler-state-table compiler-state))
        (make-node :code code)))

(defun label-for-expression (state regular-expression)
  (multiple-value-bind (node present?)
      (gethash regular-expression (compiler-state-table state))
    (if present?
        (node-label node)
        (progn
          (compile-expression-into-state state regular-expression)
          (label-for-expression state regular-expression)))))

(defun compile-expression-into-state (state regular-expression)
  (let ((classes (derivative-classes regular-expression))
        (node (add-code state regular-expression nil)))
    (setf (node-code node)
          `(let ((value (next-value ,(nullable regular-expression))))
             (cond
               ,@(loop for class in classes
                       for derivative = (derivative regular-expression class)
                       unless (set-null class)
                       collect `(,(make-test-form class 'value)
                                 (go ,(label-for-expression state derivative)))))))))

(defun make-lambda-form (regular-expression &key (vector-type 'vector))
  (let ((compiler-state (make-compiler-state)))
    (add-code compiler-state (empty-string) '(return position))
    (add-code compiler-state (empty-set) '(return nil))
    `(lambda (vector &key (start 0))
       (declare (optimize (speed 3) (safety 1)
                          (debug 0))
                (,vector-type vector)
                ((and (integer 0 *) fixnum) start)
                #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
       (macrolet ((next-value (nullable?)
                    `(if (>= position end)
                         (return ,(if nullable?
                                      'position
                                      'nil))
                         (prog1 (aref vector position)
                           (incf position)))))
         (prog ((position start)
                (end (length vector)))
            (go ,(label-for-expression compiler-state regular-expression))
            ,@(loop for node being the hash-values of (compiler-state-table compiler-state)
                    appending `(,(node-label node) ,(node-code node))))))))
