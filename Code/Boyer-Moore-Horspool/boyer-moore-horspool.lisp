(in-package :one-more-re-nightmare)

(defclass compiler:bmh-prefix-scanner (compiler:trivial-prefix-scanner)
  ((minimum-prefix-length :initarg :minimum-prefix-length
                          :initform 2
                          :reader minimum-prefix-length)))

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

(defmethod compiler:generate-starting-code
    ((scanner compiler:bmh-prefix-scanner) regular-expression aref-generator fail)
  (multiple-value-bind (prefix suffix)
      (prefix regular-expression)
    (when (< (length prefix) (minimum-prefix-length scanner))
      (return-from compiler:generate-starting-code
        (call-next-method)))
    (flet ((%aref (position)
             (funcall aref-generator compiler:*vector* position)))
      (values
       `(let ((skip ,compiler:*start*))
          (declare (alexandria:array-length skip))
          (loop
            (when (> (+ skip ,(length prefix)) ,compiler:*end*)
              ,fail)
            (when (and ,@(loop for element across prefix
                               for position from 0
                               collect `(eql ,(%aref `(the alexandria:array-length
                                                           (+ ,position skip)))
                                             ',element)))
              (setf ,compiler:*start* skip
                    ,compiler:*position* (+ skip ,(length prefix)))
              (go ,(compiler:state-name suffix)))
            (let ((element ,(%aref `(+ skip ,(1- (length prefix))))))
              (incf skip
                    ,(boyer-moore-horspool-skip-expression 'element prefix)))))
       '()))))

(setf compiler:*scanner*
      (make-instance 'compiler:bmh-prefix-scanner))
