(in-package :one-more-re-nightmare)

(defvar *bits*)
(defvar *broadcasts*)
(defun find-broadcast (value)
  (or (gethash value *broadcasts*)
      (setf (gethash value *broadcasts*)
            (make-symbol (format nil "BROADCAST-~d" value)))))

(defun test-from-isum (variable isum)
  (translate-scalar-code (make-test-form isum variable '<= '=)))

(defun code-from-prefix (prefix)
  (assert (not (null prefix)) () "Why /even bother/ with a zero-length prefix?")
  (let ((tests '())
        (loads '())
        (assignments '())
        (n 0))
    (dolist (part prefix)
      (trivia:ematch part
        ((list :literal isum)
         (let ((name (make-symbol (format nil "LOAD-~d" n))))
           (trivia:ematch (test-from-isum name isum)
             (:always)
             (:never (error "This should never happen - the empty set has no prefix!"))
             (test
              (push `(,name (,(ecase *bits*
                                (32 'one-more-re-nightmare.vector-primops:v-load32)
                                (8  'one-more-re-nightmare.vector-primops:v-load8))
                             vector
                             (the fixnum (+ ,n start))))
                    loads)
              (push test tests))))
         (incf n))
        ((list :tags tags)
         (push `(let ((position (the fixnum (+ ,n position))))
                  ,@(setf-from-assignments tags))
               assignments))))
    (values
     (reduce (lambda (a b) `(one-more-re-nightmare.vector-primops:v-and ,a ,b))
             tests)
     loads
     `(progn ,@(reverse assignments))
     n)))
