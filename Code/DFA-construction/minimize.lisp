(in-package :one-more-re-nightmare)

(defun partition (elements key)
  (let ((result (make-hash-table :test 'equal)))
    (dolist (element elements)
      (push element (gethash (funcall key element) result)))
    (alexandria:hash-table-values result)))

(defun tabulate (partitions)
  (let ((table (make-hash-table :test 'eq)))
    (loop for partition in partitions
          for n from 0
          do (dolist (element partition)
               (setf (gethash element table) n)))
    table))

(defun minimize (dfa)
  (let* ((partitions
           (partition (alexandria:hash-table-values dfa)
                      (lambda (state)
                        (if (eq (nullable (state-expression state)) (empty-set))
                            'not-nullable
                            (state-exit-map state)))))
         (table (tabulate partitions)))
    (loop
          (let ((new-partitions
                  (loop for partition in partitions
                        append (partition partition
                                          (lambda (state)
                                            (loop for transition in (state-transitions state)
                                                  collect (list (transition-class transition)
                                                                (transition-tags-to-set transition)
                                                                (gethash (transition-next-state transition)
                                                                         table))))))))
            (when (equal (length new-partitions) (length partitions))
              (return (values table (map 'vector #'first partitions))))
            (setf partitions new-partitions
                  table (tabulate partitions))))))
