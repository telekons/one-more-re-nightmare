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
    (or (gethash regular-expression table)
        (setf (gethash regular-expression table)
              (compile-regular-expression regular-expression)))))

(defun all-matches (regular-expression vector
                    &key (start 0) (end (length vector)))
  "Returns a list of (start end) of every match"
  (let ((function (find-compiled-regular-expression regular-expression
                                                    vector))
        (matches '()))
    (funcall function vector start end (lambda (start end)
                                         (push (list start end) matches)))
    (nreverse matches)))

(defun all-string-matches (regular-expression vector
                           &key (start 0) (end (length vector)))
  (let ((matches (all-matches regular-expression vector
                              :start start :end end)))
    (loop for (start end) in matches
          collect (subseq vector start end))))

(defun first-match (regular-expression vector
                    &key (start 0) (end (length vector)))
  "Returns the start and end positions of the first match, or NIL and NIL"
  (let ((function (find-compiled-regular-expression regular-expression
                                                    vector)))
    (funcall function vector start end
             (lambda (start end)
               (return-from first-match (values start end))))
    (values nil nil)))

(defun first-string-match (regular-expression vector
                           &key (start 0) (end (length vector)))
  "Returns the first match or NIL"
  (multiple-value-bind (start end)
      (first-match regular-expression vector
                   :start start :end end)
    (if (null start)
        nil
        (subseq vector start end))))
