(in-package :one-more-re-nightmare)

(defvar *bits*)
(defvar *broadcasts*)
(defun find-broadcast (value)
  (or (gethash value *broadcasts*)
      (setf (gethash value *broadcasts*)
            (make-symbol (format nil "BROADCAST-~d" value)))))

(defun test-from-isum (variable isum)
  (translate-scalar-code variable (make-test-form isum variable)))

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
              (push `(,name (,(find-op "LOAD") vector (the fixnum (+ ,n start))))
                    loads)
              (push test tests))))
         (incf n))
        ((list :tags tags)
         (push `(let ((position (the fixnum (+ ,n position))))
                  ,(setf-from-assignments tags))
               assignments))))
    (values
     (one-more-re-nightmare.vector-primops:all-of tests)
     loads
     `(progn ,@(reverse assignments))
     n)))
