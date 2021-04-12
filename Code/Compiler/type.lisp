(in-package :one-more-re-nightmare)

(defvar *instances* (make-hash-table :test 'eql))

(defmacro define-type ((name &rest slots) &key simplify hash-cons printer)
  (let ((variables (loop for slot in slots collect (gensym (symbol-name slot))))
        (internal-creator (alexandria:format-symbol nil "%~a" name)))
    `(progn
       (defclass ,name () ,slots)
       (trivia:defpattern ,name ,variables
          (alexandria:with-gensyms (instance-name)
            (list 'trivia:guard1 (list instance-name ':type ',name)
                (list 'typep instance-name '',name)
               ,@(loop for slot in slots
                       for variable in variables
                       appending `((list 'slot-value instance-name '',slot) ,variable)))))
       (setf (gethash ',name *instances*)
             (trivial-garbage:make-weak-hash-table :weakness :value
                                                   :test 'equal))
       (defun ,internal-creator ,slots
         (let ((instance (make-instance ',name)))
           ,@(loop for slot in slots
                   collect `(setf (slot-value instance ',slot) ,slot))
           instance))
       (defmethod print-object ((instance ,name) stream)
         ,(if (null printer)
              `(write (list ',name ,@(loop for slot in slots
                                           collect `(slot-value instance ',slot)))
                      :stream stream)
              `(trivia:ematch instance
                 ,printer)))
       (defun ,name ,slots
         (trivia:match (list ,@slots)
           ,@(loop for ((nil . pattern) replacement) in simplify
                   collect `((list ,@pattern) ,replacement))
           ,@(loop for ((nil . pattern) (nil . replacement)) in hash-cons
                   collect `((list ,@pattern)
                             (or (gethash (list ,@replacement)
                                          (gethash ',name *instances*))
                                 (trivia.next:next))))
           (_ (or (gethash (list ,@slots)
                           (gethash ',name *instances*))
                  (setf (gethash (list ,@slots)
                                 (gethash ',name *instances*))
                        (,internal-creator ,@slots)))))))))
(indent:define-indentation define-type (4 &body))
