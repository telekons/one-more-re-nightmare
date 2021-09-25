(in-package :one-more-re-nightmare)

(defvar *compiler-state*)
(defvar *layout*)

(defclass compiler-state ()
  ((variable-names :initform (make-hash-table :test 'equal)
                   :reader variable-names)
   (state-names    :initform (make-hash-table :test 'equal)
                   :reader state-names)
   (next-state-name :initform 0
                    :accessor next-state-name)
   (variable-map :initarg :variable-map
                 :reader variable-map)))

(defun find-variable-name (variable)
  (when (eq variable 'position)
    (return-from find-variable-name 'position))
  (let ((names (variable-names *compiler-state*)))
    (multiple-value-bind (name present?)
        (gethash variable names)
      (if present?
          name
          (setf (gethash variable names)
                (make-symbol (format nil "~{~a.~a~}" variable)))))))

(defun find-state-name (state &optional (entry-point :bounds-check))
  (let ((names (state-names *compiler-state*)))
    (multiple-value-bind (name present?)
        (gethash (cons state entry-point) names)
      (if present?
          name
          (setf (gethash (cons state entry-point) names)
                (incf (next-state-name *compiler-state*)))))))

(defun compile-regular-expression (expression &key (layout *default-layout*))
  (let ((*layout* layout))
    (with-hash-consing-tables ()
      (multiple-value-bind (expression groups)
          (parse-regular-expression expression)
        (values
         (with-naughty-compiler-switches ()
           (compile nil
                    (%compile-regular-expression
                     expression
                     *default-strategy*
                     groups)))
         groups)))))

(defun variable-map-from-groups (groups)
  (coerce `(start end ,@(alexandria:iota (* groups 2) :start 1))
          'vector))

(defun %compile-regular-expression (expression strategy groups)
  (let* ((*compiler-state*
           (make-instance 'compiler-state
                          :variable-map (variable-map-from-groups groups)))
         (macros (macros-for-strategy strategy)))
    (multiple-value-bind (variables declarations body)
        (make-prog-parts strategy expression)
      `(lambda ,(lambda-list strategy)
         (declare (optimize ,@*optimize-settings*)
                  ,@(declarations strategy)
                  #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note style-warning))
         (macrolet ,macros
           (prog* ,variables
              (declare ,@declarations)
              ,@body))))))

(defgeneric make-prog-parts (strategy expression)
  (:method (strategy expression)
    (let* ((initial-expressions (initial-states strategy expression))
           (states (make-dfa-from-expressions initial-expressions)))
      (compute-predecessor-lists states)
      (compute-minimum-lengths states)
      (let* ((body (make-body-from-dfa states))
             (initial-states (loop for expression in initial-expressions
                                   collect (gethash expression states)))
             (start-code (start-code strategy initial-states))
             (variables (alexandria:hash-table-values
                         (variable-names *compiler-state*))))
        (values
         `((start start)
           (position start)
           ,@(loop for variable in variables collect `(,variable 0)))
         `((alexandria:array-index start position ,@variables))
         (append start-code body))))))

(defun make-body-from-dfa (states)
  (loop for state being the hash-values of states
        for expression = (state-expression state)
        for nullable = (nullable expression)
        ;; Yes, we still need to test would we continue scanning the
        ;; string. 
        for minimum-length = (max (minimum-length state) 1)
        ;; We "inline" these states into transitions rather than
        ;; emitting a GO form.
        unless (or (re-stopped-p expression)
                   (re-empty-p expression))
          append
        `(,(find-state-name state :bounds-check)
          (unless (<= (the alexandria:array-index (+ position ,minimum-length)) end)
            ,(if (eq (empty-set) nullable)
                 `(return)
                 ;; We hit EOF and this state is nullable, so
                 ;; succeed with what we got so far.
                 `(progn
                    ,@(setf-from-assignments
                       (keep-used-assignments
                        nullable
                        (tags (state-expression state))))
                    (setf start position)
                    (win ,@(win-locations (state-exit-map state))))))
          ,(find-state-name state :no-bounds-check)
          (let ((value (,(layout-ref *layout*) vector position)))
            (cond
              ,@(loop for transition in (state-transitions state)
                      collect `(,(make-test-form (transition-class transition)
                                                 'value
                                                 (layout-test *layout*))
                                ,(transition-code state transition))))))))

(defun transition-code (previous-state transition)
  (let* ((next-state (transition-next-state transition))
         (next-expression (state-expression next-state)))
    (cond
      ((re-stopped-p next-expression)
       (if (eq (nullable (state-expression previous-state)) (empty-set))
           `(progn
              (setf start (1+ start))
              (go start))
           ;; Similarly to hitting EOF, if this state is nullable then
           ;; we can succeed with what we got.
           `(progn
              ,@(setf-from-assignments
                 (transition-tags-to-set transition))
              (setf start ,(find-in-map 'end (state-exit-map next-state)))
              (win ,@(win-locations (state-exit-map next-state))))))
      ((re-empty-p next-expression)
       `(progn
          ,@(setf-from-assignments
             (transition-tags-to-set transition))
          (incf position)
          ,@(setf-from-assignments
             (tags next-expression))
          (setf start position)
          (win ,@(win-locations (state-exit-map next-state)))))
      (t
       (let ((entry-point
               (if (< (minimum-length next-state)
                      (minimum-length previous-state))
                   :no-bounds-check
                   :bounds-check)))
         `(progn
            ,@(setf-from-assignments
               (transition-tags-to-set transition))
            (incf position)
            (go ,(find-state-name next-state entry-point))))))))

(defun win-locations (exit-map)
  (loop for variable-name across (variable-map *compiler-state*)
        for variable = (find variable-name exit-map :key #'first)
        if (not (null variable))
          collect `(,variable-name ,(find-variable-name variable))
        else
          collect `(,variable-name 'nil)))

(defun setf-from-assignments (assignments)
  `((setf
     ,@(loop for (variable replica source)
               in assignments
             unless (equal (list variable replica) source)
               collect (find-variable-name (list variable replica))
               and collect (find-variable-name source)))))

(defun find-in-map (variable-name map)
  (let ((variable (find variable-name map :key #'first)))
    (if (null variable)
        (error "~s not in the map ~s" variable-name map)
        (find-variable-name variable))))

(defmethod start-code ((strategy scan-everything) states)
  (destructuring-bind (state) states
    (let ((expression (state-expression state)))
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
                (incf position)
                (win ,@(win-locations
                        (loop for (variable replica nil) in effects
                              collect (list variable replica)))))))))
        (t
         `(start
           (setf position start)
           (go ,(find-state-name state :bounds-check))))))))

(defmethod start-code :around ((strategy call-continuation) states)
  (append (call-next-method)
          `(win
            (funcall continuation)
            (restart))))
