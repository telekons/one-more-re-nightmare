(in-package :one-more-re-nightmare)

(defclass strategy ()
  ()
  (:documentation "A compilation strategy describes how potential matches should be searched for."))

(defgeneric pre-process-re (strategy expression)
  (:documentation "Return a new RE with any modifications the strategy requires."))
(defgeneric lossage-code (strategy)
  (:documentation "Generate a form to insert when transitioning to the (EMPTY-SET) state."))
(defgeneric success-code (strategy)
  (:documentation "Generate a form to insert to keep searching after successfully matching."))
(defgeneric initial-states (strategy)
  (:documentation "Compute a list of states to start compiling from."))
(defgeneric make-complete-form (strategy variables declarations body)
  (:documentation "Create a Lisp form with a list of variable bindings (as by LET*), a list of declarations and TAGBODY body."))

(defclass scan-everything (strategy)
  ((initial-state :initarg :initial-state
                  :reader initial-state))
  (:documentation "A compilation strategy which runs a regular expression vector over every position."))

(defun make-search-machine (expression)
  ;; We add an ALPHA wrapper to store the last end point when we
  ;; succeed but have repetition, and a GREP wrapper to make sure we
  ;; continue when we fail to match.
  (let ((a (alpha (join (tag-set '((start 0 position)))
                        (join expression
                              (tag-set '((end 0 position)))))
                  (empty-set))))
    (grep a a)))

(defmethod pre-process-re ((strategy scan-everything) expression)
  (make-search-machine expression))

(defmethod lossage-code ((strategy scan-everything))
  (error "We generate a RE that never fails, so LOSSAGE-CODE shouldn't ever be called."))

(defmethod success-code ((strategy scan-everything))
  `(go ,(find-state-name (initial-state strategy))))

(defmethod initial-states ((strategy scan-everything))
  (list (make-search-machine (initial-state strategy))))

(defmethod make-complete-form (strategy variables declarations body)
  `(prog* ,variables
      (declare ,@declarations)
      ,@body))
