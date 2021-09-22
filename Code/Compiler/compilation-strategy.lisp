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
(defgeneric initial-states (strategy expression)
  (:documentation "Compute a list of states to start compiling from."))
(defgeneric macros-for-strategy (strategy)
  (:documentation "A list of macros (at least using including WIN and RESTART) to use for compilation.")
  (:method-combination append))
(defgeneric lambda-list (strategy)
  (:documentation "The lambda list of the function to generate."))

(defclass scan-everything (strategy)
  ()
  (:documentation "A compilation strategy which runs a regular expression vector over every position."))

(defclass call-continuation (strategy)
  ()
  (:documentation "A compilation strategy which calls a continuation when a match is found."))

(defvar *default-strategy*
  (make-instance (dynamic-mixins:mix 'scan-everything 'call-continuation)))

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

(defmethod initial-states ((strategy scan-everything) expression)
  (list (make-search-machine expression)))

(defmethod macros-for-strategy append ((strategy scan-everything))
  '((restart (next-position)
     `(progn
        (if (= ,next-position start)
            (setf start (1+ start))
            (setf start ,next-position))
        (go 1)))))

(defmethod macros-for-strategy append ((strategy call-continuation))
  '((win (&rest variables)
     `(funcall continuation
       (list
        ,@(loop for (name variable) in variables
                collect `(list ',name ,variable)))))))

(defmethod lambda-list ((strategy call-continuation))
  '(vector start end continuation))
