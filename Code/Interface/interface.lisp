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

(defmacro with-code (((function groups) code) &body body)
  `(let ((,function (car ,code))
         (,groups (cdr ,code)))
     ,@body))

(declaim (inline %all-matches))
(defun %all-matches (code vector start end)
  (declare (alexandria:array-index start end))
  (assert (and (<= end (length vector))
               (<= 0 start end)))
  (with-code ((function groups) code)
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
     (lint-regular-expression ,regular-expression)
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
  (declare (simple-vector match-vector)
           (vector vector)
           (optimize (speed 3) (safety 0)))
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
               (<= 0 start end)))
  (with-code ((function groups) code)
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

(defmacro do-matches (((&rest registers) regular-expression vector
                       &key (start 0) (end nil))
                      &body body)
  (alexandria:with-gensyms (function groups match-vector)
    (alexandria:once-only (start end)
      (flet ((consume (code vector &optional known-register-count)
               (when (and (not (null known-register-count))
                          (> (length registers) known-register-count))
                 (warn "This regular expression only produces ~r register~:p, but ~r variables were provided."
                        known-register-count
                        (length registers))
                 (setf known-register-count nil))
               `(with-code ((,function ,groups) ,code)
                  (declare (ignorable ,groups))
                  (when (null ,end)
                    (setf ,end (length ,vector)))
                  (assert (and (<= ,end (length ,vector))
                               (<= 0 ,start ,end)))
                  (let ((,match-vector (make-array ,(if (null known-register-count)
                                                        `(match-vector-size ,groups)
                                                        known-register-count))))
                    ,(if (null known-register-count)
                         `(assert (>= (length ,match-vector)
                                      ,(length registers))
                                  ()
                                  "This regular expression only produces ~r register~:p, but ~r variables were provided."
                                  (length ,match-vector)
                                  ,(length registers))
                         `(declare (dynamic-extent ,match-vector)))
                    (funcall ,function ,vector ,start ,end ,match-vector
                             (lambda ()
                               (let ,(loop for register in registers
                                           for n from 0
                                           collect `(,register (svref ,match-vector ,n)))
                                 (declare ((or null alexandria:array-index) ,@registers))
                                 ,@body)))))))
        (if (stringp regular-expression)
            (multiple-value-bind (re groups)
                (with-hash-consing-tables ()
                  (parse-regular-expression regular-expression))
              (declare (ignore re))
              (with-code-for-vector (code vector regular-expression)
                (consume code vector (match-vector-size groups))))
            (alexandria:with-gensyms (code)
              (alexandria:once-only (vector)
                `(let ((,code (find-code ,regular-expression (string-type-of ,vector))))
                   ,(consume code vector)))))))))
