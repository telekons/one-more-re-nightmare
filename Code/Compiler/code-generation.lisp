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

(defun compile-regular-expression (expression)
  (compile nil
           (%compile-regular-expression (parse-regular-expression expression)
                                        #()
                                        *default-strategy*)))

(defun %compile-regular-expression (expression variable-map strategy)
  (let* ((*compiler-state* (make-instance 'compiler-state))
         (macros (macros-for-strategy strategy)))
    (multiple-value-bind (variables declarations body)
        (make-prog-parts strategy expression variable-map)
      `(lambda ,(lambda-list strategy)
         (declare (simple-string vector)
                  (fixnum start end)
                  (function continuation)
                  (optimize (speed 3) (safety 0)))
         (macrolet ,macros
           (prog* ,variables
              (declare ,@declarations)
              ,@body))))))

(defgeneric make-prog-parts (strategy expression variable-map)
  (:method (strategy expression variable-map)
    (let ((initial-states (initial-states strategy expression)))
      (multiple-value-bind (dfa states)
          (make-dfa-from-expressions initial-states)
        (let* ((body (make-body-from-dfa dfa states))
               (start-code (start-code strategy initial-states))
               (variables (alexandria:hash-table-values
                           (variable-names *compiler-state*))))
          (values
           `((start start)
             (position start)
             ,@(loop for variable in variables collect `(,variable 0)))
           `(((and unsigned-byte fixnum) start position ,@variables))
           (append start-code body)))))))

(defun make-body-from-dfa (dfa states)
  (loop for state       being the hash-keys of dfa
        for transitions being the hash-values of dfa
        for state-info  = (gethash state states)
        for nullable    = (nullable state)
        collect (find-state-name state)
        collect `(if (< position end)
                     (let ((value (aref vector position)))
                       (cond
                         ,@(loop for transition in transitions
                                 collect `(,(make-test-form (transition-class transition)
                                                            'value)
                                           ,(transition-code state transition states)))))
                     ,(if (eq (empty-set) nullable)
                          `(return)
                          ;; We hit EOF and this state is nullable, so
                          ;; succeed with what we got so far.
                          `(progn
                             ,@(setf-from-assignments
                                (keep-used-assignments
                                 nullable
                                 (tags state)))
                             (win ,@(win-locations (state-exit-map state-info)))
                             (return))))))

(defun transition-code (previous-state transition states)
  (let* ((next-state (transition-next-state transition))
         (state-info (gethash next-state states)))
    (cond
      ((re-stopped-p next-state)
       (if (eq (nullable previous-state) (empty-set))
           `(restart start)
           ;; Similarly to hitting EOF, if this state is nullable then
           ;; we can succeed with what we got.
           `(progn
              ,@(setf-from-assignments
                 (transition-tags-to-set transition))
              (win ,@(state-exit-map state-info))
              (restart ,(find-in-map 'end (state-exit-map state-info))))))
      ((re-empty-p next-state)
       `(progn
          ,@(setf-from-assignments
             (transition-tags-to-set transition))
          (incf position)
          ,@(setf-from-assignments
             (tags next-state))
          (win ,@(win-locations (state-exit-map state-info)))
          (restart position)))
      (t
       `(progn
          ,@(setf-from-assignments
             (transition-tags-to-set transition))
          (incf position)
          (go ,(find-state-name next-state)))))))

(defun win-locations (exit-map)
  (loop for name in exit-map
        for (variable nil) = name
        collect `(,variable ,(find-variable-name name))))

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

(defun start-code (strategy expressions)
  (destructuring-bind (expression) expressions
    (cond
      ((eq expression (empty-set))
       ;; Just return immediately if we're told to match nothing.
       `(start (return)))
      ((re-empty-p expression)
       ;; Succeed for every character?
       (let ((effects (effects expression)))
         `(start
           (cond
             ((= position end)
              (return))
             (t
              ,@(setf-from-assignments effects)
              (win ,@(win-locations
                      (loop for (variable replica nil) in effects
                            collect (list variable replica))))
              (incf position)
              (go start))))))
      (t
       `(start (go ,(find-state-name expression)))))))
