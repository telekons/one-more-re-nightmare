(in-package :one-more-re-nightmare)

(defun match-vector-size (groups)
  (* 2 (1+ groups)))

(defun %all-matches (code vector start end)
  (assert (and (<= end (length vector))
               (<= start end)))
  (destructuring-bind (function groups) code
    (let ((match (make-array (match-vector-size groups)
                             :initial-element nil))
          (results '()))
      (funcall function vector start end match
               (lambda ()
                 (push (copy-seq match) results)))
      (reverse results))))
  
(defun all-matches (regular-expression vector
                    &key (start 0) (end (length vector)))
  "Returns a list of match vectors of every match"
  (%all-matches (find-code regular-expression (string-type-of vector))
                vector start end))

(defmacro with-code-for-vector ((code vector regular-expression) &body body)
  `(alexandria:once-only (,vector)
     (alexandria:with-gensyms (,code)
       `(let ((,,code
                (cond
                  ,@(loop for string-type in *string-types*
                          collect `((typep ,,vector ',string-type)
                                    (load-time-value (find-code ,,regular-expression ',string-type))))
                  (t (find-code ,,regular-expression (string-type-of ,,vector))))))
          ,(progn ,@body)))))

(define-compiler-macro all-matches (&whole w
                                    regular-expression vector
                                    &key (start 0)
                                         (end nil end-p))
  (if (not (stringp regular-expression))
      w
      ;; Grab code at load-time if possible.
      (with-code-for-vector (code vector regular-expression)
        `(%all-matches ,code ,vector ,start ,(if end-p end `(length ,vector))))))

(defun subsequences (vector match-vector)
  (declare (simple-vector match-vector))
  (let* ((sequences (floor (length match-vector) 2))
         (string-match-vector (make-array sequences)))
    (loop for n below sequences
          for start = (aref match-vector (* n 2))
          for end = (aref match-vector (1+ (* n 2)))
          if (null start)
            do (setf (aref string-match-vector n) nil)
          else
            do (setf (aref string-match-vector n)
                     (subseq vector start end)))
    string-match-vector))

(defun all-string-matches (regular-expression vector
                           &key (start 0) (end (length vector)))
  (mapcar (lambda (match) (subsequences vector match))
          (all-matches regular-expression
                       vector
                       :start start
                       :end end)))

(define-compiler-macro all-string-matches (&whole w
                                          regular-expression vector
                                          &key (start 0)
                                               (end nil end-p))
  (if (not (stringp regular-expression))
      w
      ;; Grab code at load-time if possible.
      (with-code-for-vector (code vector regular-expression)
        `(mapcar (lambda (match) (subsequences ,vector match))
                 (%all-matches ,code ,vector ,start ,(if end-p end `(length ,vector)))))))

(defun first-match (regular-expression vector
                    &key (start 0) (end (length vector)))
  "Returns the start, end positions and submatches of the first match, or NIL, NIL and NIL"
  (multiple-value-bind (function tags)
      (find-compiled-regular-expression regular-expression
                                        vector)
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
