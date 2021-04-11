(in-package :one-more-re-nightmare)

(defvar *compiled-regexps* (make-hash-table :test 'equal))

(defun vector-expression-type (vector)
  (let ((array-type (if (typep vector 'simple-array)
                        'simple-array 'array)))
    `(,array-type ,(array-element-type vector) (*))))

(defun find-regular-expression-table (type)
  (or (gethash type *compiled-regexps*)
      (setf (gethash type *compiled-regexps*)
            (make-hash-table :test 'equal))))

(defun find-compiled-regular-expression (regular-expression vector)
  (let* ((type  (vector-expression-type vector))
         (table (find-regular-expression-table type)))
    (values-list
     (or (gethash regular-expression table)
         (setf (gethash regular-expression table)
               (multiple-value-list
                (compile-regular-expression regular-expression
                                            :vector-type type)))))))

(defun all-matches (regular-expression vector
                    &key (start 0) (end (length vector)))
  "Returns a list of (start end tags) of every match"
  (multiple-value-bind (function tags)
      (find-compiled-regular-expression regular-expression
                                        vector)
    (let ((matches '())
          (tag-vector (make-array tags)))
      (funcall function vector start end tag-vector
               (lambda (start end)
                 (push (list start end (alexandria:copy-array tag-vector))
                       matches)))
      (nreverse matches))))

(declaim (inline subseqs<-positions))
(defun subseqs<-positions (vector tag-vector length)
  (declare (optimize (speed 3) (safety 0))
           (simple-vector tag-vector)
           (alexandria:array-length length))
  (let ((subseqs (make-array (floor length 2)
                             :initial-element nil)))
    (loop for n from 0
          for tag-n below length by 2
          for start = (aref tag-vector tag-n)
          for end   = (aref tag-vector (1+ tag-n))
          unless (or (null start) (null end))
            do (setf (aref subseqs n)
                     (subseq vector start end)))))

(defun all-string-matches (regular-expression vector
                           &key (start 0) (end (length vector)))
  (let ((matches (all-matches regular-expression vector
                              :start start :end end)))
    (loop for (start end tags) in matches
          collect (subseq vector start end) into matches
          collect (subseqs<-positions vector tags (length tags))
            into submatches
          finally (return (values matches submatches)))))

(defun first-match (regular-expression vector
                    &key (start 0) (end (length vector)))
  "Returns the start, end positions and submatches of the first match, or NIL, NIL and NIL"
  (multiple-value-bind (function tags)
      (find-compiled-regular-expression regular-expression
                                        vector)
    (let ((tag-vector (make-array tags)))
      (funcall function vector start end tag-vector
               (lambda (start end)
                 (return-from first-match (values start end tag-vector))))
      (values nil nil nil))))

(defun first-string-match (regular-expression vector
                           &key (start 0) (end (length vector)))
  "Returns the first match or NIL"
  (multiple-value-bind (start end)
      (first-match regular-expression vector
                   :start start :end end)
    (if (null start)
        nil
        (subseq vector start end))))
