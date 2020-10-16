(in-package :one-more-re-nightmare)

(defun bad-element-table (prefix)
  (let ((table (make-hash-table)))
    (loop with |length - 1| = (1- (length prefix))
          for character across prefix
          for position from 0 below |length - 1|
          do (setf (gethash character table)
                   (- |length - 1| position)))
    table))

(defun boyer-moore-horspool-skip-expression (variable prefix)
  "Produce an expression that will compute the skip value for a variable."
  (let ((table (bad-element-table prefix)))
    `(cond
       ,@(loop for element being the hash-keys of table
               for difference being the hash-values of table
               collect `((eql ,variable ,element) ,difference) into parts
               ;; Be optimistic and check the best skips first.
               finally (return (sort parts #'> :key #'second)))
       (t ,(length prefix)))))

(defun boyer-moore-horspool-search-expression (start length vector prefix aref-generator fail)
  (when (= 0 (length prefix))
    (return-from boyer-moore-horspool-search-expression
      `(when (>= ,start ,length)
         ,fail)))
  (flet ((%aref (position)
           (funcall aref-generator vector position)))
    `(block bmh-search
       (let ((skip ,start))
         (declare (alexandria:array-length skip))
         (loop
           (when (> (+ skip ,(length prefix)) ,length)
             (setf ,start ,length)
             ,fail)
           (when (and ,@(loop for element across prefix
                              for position from 0
                              collect `(eql ,(%aref `(the alexandria:array-length
                                                          (+ ,position skip)))
                                            ',element)))
             (setf ,start skip)
             (return-from bmh-search))
           (let ((element ,(%aref `(+ skip ,(length prefix) -1))))
             (incf skip
                   ,(boyer-moore-horspool-skip-expression 'element prefix))))))))
