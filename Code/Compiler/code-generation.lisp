(in-package :one-more-re-nightmare)

(defvar *compiler-state*)

(defclass compiler-state ()
  ((variable-names :initform (make-hash-table :test 'equal)
                   :reader variable-names)
   (state-names    :initform (make-hash-table :test 'eq)
                   :reader state-names)
   (next-state-name :initform 0
                    :accessor next-state-name)))

(defun find-variable-name (variable)
  (when (eq variable 'position)
    (return-from find-variable-name 'position))
  (let ((names (variable-names *compiler-state*)))
    (multiple-value-bind (name present?)
        (gethash variable names)
      (if present?
          name
          (setf (gethash variable names)
                (make-symbol (princ-to-string variable)))))))

(defun find-state-name (state)
  (let ((names (state-names *compiler-state*)))
    (multiple-value-bind (name present?)
        (gethash state names)
      (if present?
          name
          (setf (gethash state names)
                (incf (next-state-name *compiler-state*)))))))

(defun %compile-regular-expression (expression
                                    variable-map
                                    &key strategy)
  (let ((*compiler-state* (make-instance 'compiler-state)))
    (multiple-value-bind (variables declarations body)
        (make-prog-parts strategy expression variable-map)
      (make-complete-form strategy variables declarations body))))

(defgeneric make-prog-parts (strategy expression variable-map)
  (:method (strategy expression variable-map)
    (let ((initial-states (initial-states strategy)))
      (multiple-value-bind (dfa states)
          (make-dfa-from-expressions initial-states)
        (let* ((form (make-body-from-dfa dfa states))
               (variables (alexandria:hash-table-values
                           (variable-names *compiler-state*))))
          (values (loop for variable in variables collect `(,variable 0))
                  `((fixnum ,@variables))
                  form))))))

(defun make-body-from-dfa (dfa states)
  (loop for state       being the hash-keys of dfa
        for state-info  = (gethash state states)
        for transitions being the hash-values of dfa
        collect (find-state-name state)
        collect `(if (< position end)
                     (let ((value (aref vector position)))
                       (cond
                         ,@(loop for transition in transitions
                                 collect `(,(make-test-form (transition-class transition)
                                                            'value)
                                           ,(transition-code state transition states)))))
                     ,(if (eq (empty-set) (nullable state))
                          `(return)
                          `(progn
                             (win ,@(state-exit-map state-info))
                             (return))))))

(defun setf-from-assignments (assignments)
  (loop for (variable replica source)
          in assignments
        unless (equal (list variable replica) source)
          collect `(setf ,(find-variable-name
                           (list variable replica))
                         ,(find-variable-name source))))

(defun find-in-map (variable-name map)
  (let ((variable (find variable-name map :key #'first)))
    (if (null variable)
        (error "~s not in the map ~s" variable-name map)
        (find-variable-name variable))))

(defun transition-code (previous-state transition states)
  (let* ((next-state (transition-next-state transition))
         (state-info (gethash next-state states)))
    (cond
      ((re-stopped-p next-state)
       (if (eq (nullable previous-state) (empty-set))
           `(fail)
           `(progn
              ,@(setf-from-assignments
                 (transition-tags-to-set transition))
              (win ,@(state-exit-map state-info))
              (restart ,(find-in-map 'end (state-exit-map state-info))))))
      ((re-empty-p next-state)
       `(progn
          (win ,@(state-exit-map state-info))
          (restart position)))
      (t
       `(progn
          ,@(setf-from-assignments
             (transition-tags-to-set transition))
          (incf position)
          (go ,(find-state-name next-state)))))))
