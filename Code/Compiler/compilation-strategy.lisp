(in-package :one-more-re-nightmare)

(defclass strategy ()
  ()
  (:documentation "A compilation strategy describes how potential matches should be searched for."))

(defgeneric initial-states (strategy expression)
  (:documentation "Compute a list of states to start compiling from."))
(defgeneric macros-for-strategy (strategy)
  (:documentation "A list of macros (at least using including WIN and RESTART) to use for compilation.")
  (:method-combination append))
(defgeneric lambda-list (strategy)
  (:documentation "The lambda list of the function to generate."))
(defgeneric start-code (strategy states)
  (:documentation "Part of a TAGBODY body used to start running a DFA."))
(defgeneric declarations (strategy))

(defclass scan-everything (strategy)
  ()
  (:documentation "A compilation strategy which runs a regular expression vector over every position."))

(defclass call-continuation (strategy)
  ()
  (:documentation "A compilation strategy which calls a continuation when a match is found."))

(defun make-default-strategy (layout expression)
  (declare (ignore layout expression))
  (make-instance (dynamic-mixins:mix 'scan-everything 'call-continuation)))

(defun add-tags (expression)
  (join (tag-set '((start 0 position)))
        (join expression
               (tag-set '((end 0 position))))))

(defun make-search-machine (expression)
  ;; We add an ALPHA wrapper to store the last end point when we
  ;; succeed but have repetition, and a GREP wrapper to make sure we
  ;; continue when we fail to match.
  (let ((a (alpha (add-tags expression) (empty-set))))
    (grep a a)))

(defmethod initial-states ((strategy scan-everything) expression)
  (list (make-search-machine expression)))

(defmethod macros-for-strategy append ((strategy scan-everything))
  '((restart ()
     '(go start))))

(defmethod macros-for-strategy append ((strategy call-continuation))
  '((win (&rest variables)
     `(progn
        ,@(loop for (nil variable) in variables
                for n from 0
                collect `(setf (svref result-vector ,n) ,variable))
        (go win)))))

(defmethod lambda-list ((strategy call-continuation))
  '(vector start end result-vector continuation))

(defmethod declarations ((strategy call-continuation))
  `((,(layout-array-type *layout*) vector)
    (alexandria:array-index start end)
    (function continuation)
    (simple-vector result-vector)))
