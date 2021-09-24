(in-package :one-more-re-nightmare)

(defun all-tags (states)
  (let ((table (make-hash-table)))
    (maphash (lambda (ex state)
               (declare (ignore state))
               (dolist (assignment (tags ex))
                 (destructuring-bind (variable replica source)
                     assignment
                   (declare (ignore source))
                   (pushnew replica (gethash variable table)))))
             states)
    table))

(defun transition-value-of-variable (transition variable)
  (loop for (v r s) in (transition-tags-to-set transition)
        when (equal (list v r) variable)
          return s
        finally (return ':previous)))

(defun replacable (variable new-replica old-replica states)
  ;; We can replace one replica with the other iff there is no
  ;; transition where both are live and they have different values.
  (let ((old-variable (list variable old-replica))
        (new-variable (list variable new-replica)))
    (maphash (lambda (ex state)
               (declare (ignore ex))
               (dolist (transition (state-transitions state))
                 (let ((used (used-tags (state-expression (transition-next-state transition)))))
                   (when (and (member old-variable used :test #'equal)
                              (member new-variable used :test #'equal)
                              (not (equal (transition-value-of-variable transition old-variable)
                                          (transition-value-of-variable transition new-variable))))
                     (return-from replacable nil)))))
             states))
  t)

(defun replace-replica-in-states (variable new-replica old-replica states)
  (let ((states-to-put-back '()))
    (maphash (lambda (expression state)
               (setf (state-exit-map state)
                     (substitute (list variable new-replica)
                                 (list variable old-replica)
                                 (state-exit-map state)
                                 :test #'equal))
               (let ((new-expression
                       (replace-replica variable new-replica old-replica expression)))
                 (setf (state-expression state) new-expression)
                 (dolist (transition (state-transitions state))
                   (setf (transition-tags-to-set transition)
                         (remove-duplicates
                          (replace-replica-in-tags variable new-replica old-replica
                                                   (transition-tags-to-set transition))
                          :test #'equal)))
                 (unless (eq new-expression expression)
                   ;; We changed the expression and thus have to re-insert.
                   (push state states-to-put-back)
                   (remhash expression states))))
             states)
    (dolist (state states-to-put-back)
      (setf (gethash (state-expression state) states) state))
    (assert (not (member old-replica (gethash variable (all-tags states)))))))

(defun remove-aliases (states)
  (tagbody
   again
     (maphash (lambda (variable replicas)
                (loop for (replica . other-replicas) on (sort (copy-list replicas) #'<)
                      do (loop for other-replica in other-replicas
                               when (replacable variable replica other-replica states)
                                 do (replace-replica-in-states variable replica other-replica states)
                                    #+(or)
                                    (format t "~&found (~s ~s) = (~s ~s)"
                                            variable replica variable other-replica)
                                    (go again))))
              (all-tags states))))
