(in-package :one-more-re-nightmare)

(defstruct gvn-set
  number
  position-delta
  registers)

(defvar *gvn-gensym-counter*)
(defvar *initial-gvn-pool* '())
(defun fresh-gvn-number ()
  (incf *gvn-gensym-counter*)
  *gvn-gensym-counter*)

(defun compute-global-value-numbers (initial-states dfa)
  (let ((work-list (loop for state in initial-states
                         collect (gethash state dfa)))
        (input-pools (make-hash-table))
        (output-pools (make-hash-table))
        (exit-pools (make-hash-table))
        (*gvn-gensym-counter* 0))
    ;; Iterate until we reach a fixed point.
    (labels ((update-successors (state)
               (dolist (transition (state-transitions state))
                 (pushnew (transition-next-state transition) work-list)))
             (available-incoming-pools (state)
               (loop for transition in (incoming-transitions state)
                     when (nth-value 1 (gethash transition output-pools))
                       collect (gethash transition output-pools)))
             (has-input-pool-p (state)
               (nth-value 1 (gethash state input-pools)))
             (input (state)
               (gethash state input-pools)))
      (loop
        (when (null work-list)
          (return))
        (let* ((state (pop work-list))
               (pools (available-incoming-pools state))
               (confluence (if (null pools)
                               *initial-gvn-pool*
                               (reduce #'gvn-confluence pools))))
          (unless (and (has-input-pool-p state)
                       (gvn-similar confluence (input state)))
            #+(or)
            (format t "~&updating ~s~&from ~:s~&to ~:s~&due to ~:s"
                    state (input state) confluence pools)
            (setf (gethash state input-pools) confluence)
            (dolist (transition (state-transitions state))
              (setf (gethash transition output-pools)
                    (gvn-transfer (transition-tags-to-set transition)
                                  confluence)))
            (setf (gethash state exit-pools)
                  (gvn-transfer (state-exit-effects state) confluence))
            (update-successors state)))))
    (values input-pools output-pools exit-pools)))

(defun gvn-confluence (pool1 pool2)
  (let ((new-pool '()))
    (dolist (set1 pool1)
      (dolist (set2 pool2)
        (let ((both (intersection (gvn-set-registers set1)
                                  (gvn-set-registers set2)
                                  :test #'equal)))
          (unless (null both)
            (push (make-gvn-set
                   ;; Reuse a number if possible.
                   :number (if (and (= (gvn-set-number set1)
                                       (gvn-set-number set2))
                                    (eq (gvn-set-registers set1)
                                        (gvn-set-registers set2)))
                               (gvn-set-number set1)
                               (fresh-gvn-number))
                   :position-delta (if (eql (gvn-set-position-delta set1)
                                            (gvn-set-position-delta set2))
                                       (gvn-set-position-delta set1)
                                       nil)
                   :registers both)
                  new-pool)))))
    new-pool))

(defun find-set-in-pool (register pool)
  (find register pool
        :test (lambda (e set)
                (find e (gvn-set-registers set)
                      :test #'equal))))

(defun gvn-transfer (assignments pool)
  (flet ((bump-set (set)
           (make-gvn-set
            :number (gvn-set-number set)
            :position-delta (if (null (gvn-set-position-delta set))
                                nil
                                (1+ (gvn-set-position-delta set)))
            :registers (gvn-set-registers set))))
    (let ((new-pool (mapcar #'bump-set pool))
          (position-set (make-gvn-set
                         :number (fresh-gvn-number)
                         :position-delta 0
                         :registers '())))
      (push position-set new-pool)
      (labels ((find-set (register &key (error-p t))
                 (or (find-set-in-pool register new-pool)
                     (if error-p (error "No set for register ~s." register) nil)))
               (remove-info (register)
                 (let ((set (find-set register :error-p nil)))
                   (unless (null set)
                     (alexandria:removef (gvn-set-registers set)
                                         register
                                         :test #'equal))))
               (associate-with-position (register)
                 (push register (gvn-set-registers position-set)))
               (associate-registers (new existing)
                 (push new (gvn-set-registers (find-set existing)))))
        (loop for (register . source) in assignments
              unless (equal register source)
                do (remove-info register)
                   (if (eql source 'position)
                       (associate-with-position register)
                       (associate-registers register source)))
        (remove '() new-pool :key #'gvn-set-registers)))))

(defun gvn-similar (pool1 pool2)
  (unless (= (length pool1) (length pool2))
    (return-from gvn-similar nil))
  (let ((remaining-in-pool2 pool2))
    (dolist (set pool1)
      (flet ((similar-set-p (set2)
               (and (eql (gvn-set-position-delta set)
                         (gvn-set-position-delta set2))
                    (null (set-exclusive-or
                           (gvn-set-registers set)
                           (gvn-set-registers set2)
                           :test #'equal)))))
        (let ((similar-set (find-if #'similar-set-p remaining-in-pool2)))
          (when (null similar-set)
            (return-from gvn-similar nil))
          (alexandria:removef remaining-in-pool2 similar-set)))))
  t)

(defun do-global-value-numbering (initial-states dfa)
  "Replace each register in DFA with value numbers."
  (multiple-value-bind (in out exit)
      (compute-global-value-numbers initial-states dfa)
    ;; After computing global value numbers, we need to "translate"
    ;; every register to a value number (or an expression of the form
    ;; (- POSITION N) for natural N).
    (labels ((gvn-translate-set (set)
               (if (null (gvn-set-position-delta set))
                   `(value ,(gvn-set-number set))
                   `(- position ,(gvn-set-position-delta set))))
             (gvn-translate (register pool)
               (if (member register '(nil position))
                   register
                   (gvn-translate-set (find-set-in-pool register pool))))
             ;; We don't need to assign anything if our target value
             ;; can be written as a (- POSITION N) form.
             (implicit-target-p (target)
               (trivia:match target
                 ((list '- 'position _) t)
                 (_ nil)))
             ;; Note that OUT comes before IN, as we are working
             ;; between the output of a predecessor state, and the
             ;; input of a successor state.
             (translate (assignments out in)
               (loop for (target . source) in assignments
                     for target* = (gvn-translate target out)
                     for source* = (gvn-translate source in)
                     unless (or (implicit-target-p target*) (equal target* source*))
                       collect (cons target* source*)))
             (translate-exit-map (assignments exit)
               (loop for (target . source) in assignments
                     for source* = (gvn-translate source exit)
                     collect (cons target source*)))
             (add-gvn-translation (trans out)
               (loop with in = (gethash (transition-next-state trans) in)
                     for to-set in in
                     for witness = (first (gvn-set-registers to-set))
                     for from-set = (find-set-in-pool witness out)
                     when (and (null (gvn-set-position-delta to-set))
                               (or (/= (gvn-set-number from-set)
                                       (gvn-set-number to-set))
                                   (not (null (gvn-set-position-delta from-set)))))
                       collect (cons (gvn-translate-set to-set)
                                     (gvn-translate-set from-set))
                         into assignments
                     finally (setf (transition-tags-to-set trans)
                                   (append (transition-tags-to-set trans)
                                           assignments)))))
       (maphash (lambda (expression state)
                  (declare (ignore expression))
                  (let ((in (gethash state in)))
                    (dolist (transition (state-transitions state))
                      (let ((out (gethash transition out)))
                        (setf (transition-tags-to-set transition)
                              (translate (transition-tags-to-set transition)
                                         out in))
                        (add-gvn-translation transition out)))
                    (let ((exit (gethash state exit)))
                      (setf (state-exit-effects state)
                            (translate (state-exit-effects state)
                                       exit in)
                            (state-exit-map state)
                            (translate-exit-map (state-exit-map state)
                                                exit)))))
                dfa))))

(defun test-gvn (expression)
  (with-hash-consing-tables ()
    (let* ((re (make-search-machine (parse-regular-expression expression)))
           (dfa (make-dfa-from-expression re)))
      (compute-predecessor-lists dfa)
      (do-global-value-numbering (list re) dfa)
      dfa)))
