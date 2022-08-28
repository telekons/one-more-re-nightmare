(in-package :one-more-re-nightmare)

(defun re-groups (regular-expression)
  (nth-value 1
             (with-hash-consing-tables ()
               (parse-regular-expression regular-expression))))

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

(defmacro with-code (((function size) code) &body body)
  `(let ((,function (car ,code))
         (,size (cdr ,code)))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun constant-safe-to-eval-p (form)
    (trivia:match form
      ((or (type string) (type symbol) (list 'quote (type string))) t)
      (_ nil)))
  
  (defun try-to-evaluate-constant-re (form)
    (if (and (constantp form)
             (constant-safe-to-eval-p form))
        (let ((result (eval form)))
          (if (stringp result)
              result
              nil))
        nil)))

(define-compiler-macro compile-regular-expression (&whole w expression)
  (let ((re (try-to-evaluate-constant-re expression)))
    (cond
      ((null re) w)
      (t
       (handler-case
           (lint-regular-expression re)
         (error (e)
           (warn "Error while linting:~%~a" e)
           w)
         (:no-error (&rest values)
           (declare (ignore values))
           `(make-compiled-regular-expression
             :codes (vector ,@(loop for type in *string-types*
                                    collect `(find-code ,re ',type)))
             :original-re ',re)))))))

(defmacro with-code-for-vector ((function size vector regular-expression bailout-form) &body body)
  (alexandria:with-gensyms (result)
    `(let ((,result (try-to-evaluate-constant-re ,regular-expression)))
       (cond
         ((null ,result)
          ,bailout-form)
         (t
          (handler-case
              (lint-regular-expression ,result)
            (error (e)
              (warn "Error while linting:~%~a" e)
              ,bailout-form)
            (:no-error (&rest values)
              (declare (ignore values))
              (alexandria:once-only (,vector)
                (alexandria:with-gensyms (,function ,size)
                  `(let ((,,size ,(match-vector-size (re-groups ,result)))
                         (,,function
                           (cond
                             ,@(loop for string-type in *string-types*
                                     collect `((typep ,,vector ',string-type)
                                               (load-time-value (car (find-code ,,result ',string-type)))))
                             (t (car (find-code ,,result (string-type-of ,,vector)))))))
                     ,(progn ,@body)))))))))))

(declaim (inline %all-matches)
         (ftype (function * list) %all-matches))
(defun %all-matches (function size vector start end)
  (declare (alexandria:array-index start end)
           (function function)
           (fixnum size))
  (assert (and (<= end (length vector))
               (<= 0 start end)))
  ;; The code function will fill in values as needed.
  (let ((match (make-array size)))
    (collect (result)
      (funcall function vector start end match
               (lambda ()
                 (result (copy-seq match)))))))

(declaim (ftype (function
                 (re-designator string
                  &key (:start alexandria:array-index)
                       (:end (or alexandria:array-length null)))
                 list)
                all-matches all-string-matches))

(defun all-matches (regular-expression vector
                    &key (start 0) (end (length vector)))
  "Find every match, as a list of match vectors."
  (with-code ((function size)
              (find-code regular-expression (string-type-of vector)))
    (%all-matches function size vector start end)))

(define-compiler-macro all-matches (&whole w
                                    regular-expression vector
                                    &key (start 0)
                                         (end nil end-p))
  ;; Grab code at load-time if possible.
  (with-code-for-vector (function size vector regular-expression w)
    `(%all-matches ,function ,size ,vector
                   ,start ,(if end-p end `(length ,vector)))))

(declaim (inline subsequences))
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
  "Find every match, as a list of match string vectors."
  (mapcar (lambda (match) (subsequences vector match))
          (all-matches regular-expression
                       vector
                       :start start
                       :end end)))

(define-compiler-macro all-string-matches (&whole w
                                          regular-expression vector
                                          &key (start 0)
                                               (end nil end-p))
  ;; Grab code at load-time if possible.
  (with-code-for-vector (function size vector regular-expression w)
    `(mapcar (lambda (match) (subsequences ,vector match))
             (%all-matches ,function ,size ,vector
                           ,start ,(if end-p end `(length ,vector))))))

(declaim (inline %first-match))
(defun %first-match (function size vector start end)
  (declare (alexandria:array-index start end)
           (fixnum size)
           (function function))
  (assert (and (<= end (length vector))
               (<= 0 start end)))
  (let ((tag-vector (make-array size)))
    (funcall function vector start end tag-vector
             (lambda ()
               (return-from %first-match tag-vector)))
    nil))

(declaim (ftype (function
                 (re-designator string
                  &key (:start alexandria:array-index)
                       (:end (or alexandria:array-length null)))
                 (or null simple-vector))
                first-match first-string-match))

(defun first-match (regular-expression vector
                    &key (start 0) (end (length vector)))
  "Find the first match, returning a match vector, or NIL."
  (with-code ((function size)
              (find-code regular-expression (string-type-of vector)))
    (%first-match function size vector start end)))

(define-compiler-macro first-match (&whole w
                                    regular-expression vector
                                    &key (start 0) (end nil end-p))
  (with-code-for-vector (function size vector regular-expression w)
    `(%first-match ,function ,size ,vector ,start ,(if end-p end `(length ,vector)))))

(defun first-string-match (regular-expression vector
                           &key (start 0) (end (length vector)))
  "Find the first match, returning a match string vector or NIL"
  (let ((results (first-match regular-expression vector
                              :start start :end end)))
    (if (null results)
        nil
        (subsequences vector results))))

(define-compiler-macro first-string-match (&whole w
                                           regular-expression vector
                                           &key (start 0) (end nil end-p))
  (with-code-for-vector (function size vector regular-expression w)
    `(subsequences ,vector
                   (%first-match ,function ,size ,vector
                                 ,start ,(if end-p end `(length ,vector))))))

(defmacro do-matches (((&rest registers) regular-expression vector
                       &key (start 0) (end nil))
                      &body body)
  "Iterate over every match, binding match registers."
  (alexandria:with-gensyms (function size match-vector)
    (alexandria:once-only (start end vector)
      (labels ((consume (function size known-register-count)
                 (when (and (not (null known-register-count))
                            (> (length registers) known-register-count))
                   (warn "This regular expression only produces ~r register~:p, but ~r variables were provided."
                         known-register-count
                         (length registers))
                   (setf known-register-count nil))
                 `(progn
                    (when (null ,end)
                      (setf ,end (length ,vector)))
                    (assert (and (<= ,end (length ,vector))
                                 (<= 0 ,start ,end)))
                    (let ((,match-vector (make-array ,size)))
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
                                   ,(if (>= (length registers) 2)
                                        `(declare ((or null alexandria:array-index) ,@(subseq registers 2))
                                                  (alexandria:array-index ,@(subseq registers 0 2)))
                                        `(declare (alexandria:array-index ,@registers)))
                                   ,@body))))))
               (fallback ()
                 (alexandria:once-only (vector)
                   `(with-code ((,function ,size)
                                (find-code ,regular-expression (string-type-of ,vector)))
                      ,(consume function size nil)))))
        (with-code-for-vector (function size vector regular-expression (fallback))
          (consume function size
                   (match-vector-size (re-groups regular-expression))))))))
