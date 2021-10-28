(in-package :one-more-re-nightmare)

(declaim (inline match-vector-size))
(defun match-vector-size (groups)
  (declare ((unsigned-byte 32) groups))
  (* 2 (1+ groups)))

(defmacro collect ((function) &body body)
  (alexandria:with-gensyms (list tail)
    `(let* ((,list (list 'nil))
            (,tail ,list))
       (flet ((,function (element)
                (let ((new-tail (list element)))
                  (setf (cdr ,tail) new-tail
                        ,tail new-tail))))
         (declare (dynamic-extent #',function)
                  (inline ,function))
         ,@body
         (cdr ,list)))))

(declaim (inline %all-matches))
(defun %all-matches (code vector start end)
  (declare (alexandria:array-index start end))
  (assert (and (<= end (length vector))
               (<= start end)))
  (destructuring-bind (function groups) code
    ;; The code function will fill in values as needed. 
    (let ((match (make-array (match-vector-size groups))))
      (collect (result)
        (funcall function vector start end match
                 (lambda ()
                   (result (copy-seq match))))))))
  
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

(declaim (inline %first-match))
(defun %first-match (code vector start end)
  (declare (alexandria:array-index start end))
  (assert (and (<= end (length vector))
               (<= start end)))
  (destructuring-bind (function groups) code
    (let ((tag-vector (make-array (match-vector-size groups))))
      (funcall function vector start end tag-vector
               (lambda ()
                 (return-from %first-match tag-vector)))
      nil)))

(defun first-match (regular-expression vector
                    &key (start 0) (end (length vector)))
  "Returns the start, end positions and submatches of the first match, or NIL, NIL and NIL"
  (%first-match (find-code regular-expression (string-type-of vector))
                vector start end))

(define-compiler-macro first-match (&whole w
                                    regular-expression vector
                                    &key (start 0) (end nil end-p))
  (if (stringp regular-expression)
      (with-code-for-vector (code vector regular-expression)
        `(%first-match ,code ,vector ,start ,(if end-p end `(length ,vector))))
      w))

(defun first-string-match (regular-expression vector
                           &key (start 0) (end (length vector)))
  "Returns the first match or NIL"
  (let ((results (first-match regular-expression vector
                              :start start :end end)))
    (if (null results)
        nil
        (subseq vector (svref results 0) (svref results 1)))))

(define-compiler-macro first-string-match (&whole w
                                           regular-expression vector
                                           &key (start 0) (end nil end-p))
  (if (stringp regular-expression)
      (with-code-for-vector (code vector regular-expression)
        `(subsequences ,vector
                       (%first-match ,code ,vector ,start ,(if end-p end `(length ,vector)))))
      w))
