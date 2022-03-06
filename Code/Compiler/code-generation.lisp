(in-package :one-more-re-nightmare)

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
  (when (member variable '(nil position))
    (return-from find-variable-name variable))
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

(defvar *nowhere* (make-broadcast-stream))
(defun compile-regular-expression (expression
                                   &key (layout *default-layout*)
                                        (strategy #'make-default-strategy))
  (let ((*tag-gensym-counter* 0))
    (with-hash-consing-tables ()
      (multiple-value-bind (expression groups)
          (parse-regular-expression expression)
        (let ((*layout* layout)
              (*error-output* *nowhere*)
              (strategy (funcall strategy layout expression)))
          (values
           (with-naughty-compiler-switches ()
             (compile nil
                      (%compile-regular-expression
                       expression
                       strategy
                       groups)))
           groups))))))

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
      (let* ((body (make-body-from-dfa strategy states))
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

(defun make-body-from-dfa (strategy states)
  (loop for state being the hash-values of states
        for expression = (state-expression state)
        for nullable = (nullable expression)
        ;; We "inline" these states into transitions rather than
        ;; emitting code for separate states, because they are simple
        ;; enough.
        unless (or (re-stopped-p expression)
                   (re-empty-p expression)
                   (state-never-succeeds-p state))
          append
        `(,(find-state-name state :bounds-check)
          (when (> (the alexandria:array-index
                        (+ position ,(max (minimum-length state) 1)))
                   end)
            (when (> position end)
              (return))
            ,(if (eq (empty-set) nullable)
                 `(return)
                 ;; We hit EOF and this state is nullable, so
                 ;; succeed with what we got so far.
                 `(progn
                    ;; See below for commentary on why we have to
                    ;; nudge register values around.
                    (let ((position (1+ position)))
                      ,(setf-from-assignments
                        (state-exit-effects state)))
                    (setf start (max position (1+ start)))
                    (win ,@(win-locations (state-exit-map state))))))
          ,(find-state-name state :no-bounds-check)
          #+print-traces
          (print (list position
                       ,(prin1-to-string expression)
                       ,(minimum-length state)))
          (let ((value (,(layout-to-number *layout*)
                        (,(layout-ref *layout*) vector position))))
            ;; We assign early so that the ADD instruction doesn't
            ;; force our Lisp compiler to create a new basic block,
            ;; and can just JMP directly to the next state when no tag
            ;; assignments are done. This means that every register is
            ;; going to have a value that is 1 too high, but we can
            ;; just subtract in the (infrequent) places we read
            ;; registers.
            (incf position)
            ,(let ((labels (loop for nil in (state-transitions state)
                                 for n from 0
                                 collect (alexandria:format-symbol nil "TRANSITION-~d" n))))
            `(tagbody
                (isum-case value
                    ,(layout-less *layout*)
                    ,(layout-equal *layout*)
                  ,@(loop for transition in (state-transitions state)
                          for label in labels
                          collect `(,(transition-class transition)
                                    (go ,label))))
                ,@(loop for transition in (state-transitions state)
                        for label in labels
                        collect label
                        collect (transition-code strategy state transition))))))))

(defmethod transition-code (strategy previous-state transition)
  (declare (ignore strategy))
  (let* ((next-state (transition-next-state transition))
         (next-expression (state-expression next-state)))
    (cond
      ((or (state-never-succeeds-p next-state)
           (re-stopped-p next-expression))
       (if (eq (nullable (state-expression previous-state)) (empty-set))
           `(progn
              (setf start (1+ start))
              (go start))
           ;; Similarly to hitting EOF, if this state is nullable then
           ;; we can succeed with what we got.
           `(progn
              ,(setf-from-assignments
                (transition-tags-to-set transition))
              (setf start (max (1+ start)
                               (1- ,(find-in-map 'end (state-exit-map next-state)))))
              (win ,@(win-locations (state-exit-map next-state))))))
      ((re-empty-p next-expression)
       `(progn
          ,(setf-from-assignments
            (transition-tags-to-set transition))
          ;; These assignments are evaluated as if we were at the
          ;; empty state -- we inline the empty state because it is
          ;; only a set of assignments and a call to WIN.
          (let ((position (1+ position)))
            ,(setf-from-assignments
              (tags next-expression)))
          (setf start position)
          (win ,@(win-locations (state-exit-map next-state)))))
      (t
       (let ((entry-point
               (if (and (< (minimum-length next-state)
                           (minimum-length previous-state))
                        (plusp (minimum-length next-state)))
                   :no-bounds-check
                   :bounds-check)))
         `(progn
            ,(setf-from-assignments
              (transition-tags-to-set transition))
            (go ,(find-state-name next-state entry-point))))))))

(defun win-locations (exit-map)
  (loop for variable-name across (variable-map *compiler-state*)
        for (variable . source) = (find variable-name exit-map :key #'caar)
        if (and (not (null variable))
                (not (eql source 'nil)))
          collect `(,variable-name ,(find-variable-name source))
        else
          collect `(,variable-name 'nil)))

(defun setf-from-assignments (assignments)
  `(setf
    ,@(loop for (target . source) in assignments
            ;; NIL sources are basically just a compile time thing.
            unless (or (equal target source)
                       (eql source 'nil))
              collect (find-variable-name target)
              and collect (find-variable-name source))))

(defun find-in-map (variable-name map)
  (let ((variable (find variable-name map :key #'caar)))
    (if (null variable)
        (error "~s not in the map ~s" variable-name map)
        (find-variable-name (cdr variable)))))

(defmethod start-code ((strategy scan-everything) states)
  (destructuring-bind (state) states
    (let ((expression (state-expression state)))
      (cond
        ((state-never-succeeds-p state)
         ;; Just return immediately if we're told to match nothing.
         `(start (return)))
        ((re-empty-p expression)
         ;; Succeed for every character?
         `(start
           (cond
             ((> position end)
              (return))
             (t
              (incf position)
              ,(setf-from-assignments (state-exit-effects state))
              (win ,@(win-locations (state-exit-map state)))))))
        (t
         `(start
           (setf position start)
           (go ,(find-state-name state :bounds-check))))))))

(defmethod start-code :around ((strategy call-continuation) states)
  ;; Calling the continuation gets its own state as to influence the
  ;; register allocator less. This change does actually reduce the
  ;; number of spills substantially (at least on SBCL).
  (append (call-next-method)
          `(win
            (funcall continuation)
            (restart))))
