(in-package :one-more-re-nightmare)

;;; The DFA we are printing.
(defvar *dfa*)
(defvar *initial-state*)

(defun print-dfa (dfa initial-state)
  (let ((*dfa* dfa)
        (*initial-state* (gethash initial-state dfa)))
    (cl-dot:print-graph
     (cl-dot:generate-graph-from-roots
      'dfa
      (list (gethash initial-state dfa))
      '(:node (:fontname "Inconsolata" :shape :box)
        :edge (:fontname "Inconsolata"))))))

(defmethod cl-dot:graph-object-node ((graph (eql 'dfa)) (state (eql 'nothing)))
  (make-instance 'cl-dot:node
    :attributes (list :label " "
                      :color "#00000000"
                      :fillcolor "#00000000")))

(defmethod cl-dot:graph-object-node ((graph (eql 'dfa)) state)
  ;; We add a newline to state names so that cl-dot will emit the
  ;; trailing \l, which is necessary even when there isn't another
  ;; line break.
  (make-instance 'cl-dot:node
    :attributes (list :label (list :left (format nil "~A~%" (state-expression state))))))

(defun escape-string (string)
  "A lazy way to make a single backslash into two, as cl-dot won't do it."
  (let ((printed (prin1-to-string string)))
    (subseq printed 1 (1- (length printed)))))

(defmethod cl-dot:graph-object-edges ((graph (eql 'dfa)))
  (let ((edges (list (list 'nothing *initial-state*))))
    (maphash (lambda (re state)
               (declare (ignore re))
               (dolist (transition (state-transitions state))
                 (push (list state (transition-next-state transition)
                             (list :label
                                   (escape-string
                                    (format nil "~a ~a"
                                            (transition-class transition)
                                            (tag-set
                                             (transition-tags-to-set transition))))))
                       edges)))
             *dfa*)
    edges))
