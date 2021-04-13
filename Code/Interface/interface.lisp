(in-package :one-more-re-nightmare)

;;; A hash-table of (regular-expression . class) → (function . tag-count)
(defvar *compiled-regexps* (make-hash-table :test 'equal))
(declaim ((simple-vector 4) *last-used-regexp*))
(defvar *last-used-regexp* (vector nil nil nil 0))

(defun vector-expression-type (vector)
  (let ((array-type (if (typep vector 'simple-array)
                        'simple-array 'array)))
    `(,array-type ,(array-element-type vector) (*))))

(declaim (inline set-last-used-regexp))
(defun set-last-used-regexp (regular-expression class code tags)
  (setf (svref *last-used-regexp* 0) regular-expression
        (svref *last-used-regexp* 1) class
        (svref *last-used-regexp* 2) code
        (svref *last-used-regexp* 3) tags))

(defun find-compiled-regular-expression (regular-expression vector)
  (let ((class (class-of vector)))
    (when (and (eq (svref *last-used-regexp* 0) regular-expression)
               (eq (svref *last-used-regexp* 1) class))
      (return-from find-compiled-regular-expression
        (values (svref *last-used-regexp* 2)
                (svref *last-used-regexp* 3))))
    (let ((key (cons class regular-expression)))
      (multiple-value-bind (information present?)
          (gethash key *compiled-regexps*)
        (when present?
          (set-last-used-regexp regular-expression class
                                (car information) (cdr information))
          (return-from find-compiled-regular-expression
            (values (car information) (cdr information)))))
      (multiple-value-bind (code tags)
          (compile-regular-expression
           regular-expression
           :vector-type (vector-expression-type vector))
        (setf (gethash key *compiled-regexps*) (cons code tags))
        (values code tags)))))

(defvar *empty-vector* #())

(defun all-matches (regular-expression vector
                    &key (start 0) (end (length vector)))
  "Returns a list of (start end tags) of every match"
  (multiple-value-bind (function tags)
      (find-compiled-regular-expression regular-expression
                                        vector)
    (declare (function function))
    (let ((matches '())
          (tag-vector (if (zerop tags)
                          *empty-vector*
                          (make-array tags))))
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
                     (subseq vector start end)))
    subseqs))

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
    (declare (function function))
    (let ((tag-vector (if (zerop tags)
                          *empty-vector*
                          (make-array tags))))
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
