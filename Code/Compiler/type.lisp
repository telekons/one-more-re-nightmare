(in-package :one-more-re-nightmare)

(defvar *table-names* '())

(defmacro define-hash-consing-table (name)
  `(progn
     (defvar ,name)
     (pushnew ',name *table-names*)
     ',name))

(defconstant +uncomputed+ '+uncomputed+)
(defclass regular-expression ()
  ((%nullable :initform +uncomputed+ :accessor cached-nullable)
   (%used-tags :initform +uncomputed+ :accessor cached-used-tags)
   (%tags :initform +uncomputed+ :accessor cached-tags)
   (%removed-tags :initform +uncomputed+ :accessor cached-removed-tags)
   (%has-tags-p :initform +uncomputed+ :accessor cached-has-tags-p)))

(defmacro define-type ((name &rest slots) &key simplify hash-cons printer)
  (let ((variables (loop for slot in slots collect (gensym (symbol-name slot))))
        (internal-creator (alexandria:format-symbol nil "%~a" name))
        (table-name (alexandria:format-symbol '#:one-more-re-nightmare
                                              "*~A-TABLE*" name)))
    `(progn
       (defclass ,name (regular-expression) ,slots)
       (trivia:defpattern ,name ,variables
          (alexandria:with-gensyms (instance-name)
            (list 'trivia:guard1 (list instance-name ':type ',name)
                (list 'typep instance-name '',name)
               ,@(loop for slot in slots
                       for variable in variables
                       appending `((list 'slot-value instance-name '',slot) ,variable)))))
       (define-hash-consing-table ,table-name)
       
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
                             (or (gethash (list ,@replacement) ,table-name)
                                 (trivia.next:next))))
           (_ (or (gethash (list ,@slots) ,table-name)
                  (setf (gethash (list ,@slots) ,table-name)
                        (,internal-creator ,@slots)))))))))
(indent:define-indentation define-type (4 &body))

(defmacro with-hash-consing ((table key) &body body)
  (alexandria:once-only (table key)
    (alexandria:with-gensyms (value present?)
      `(multiple-value-bind (,value ,present?)
           (gethash ,key ,table)
         (if ,present?
             ,value
             (setf (gethash ,key ,table)
                   (progn ,@body)))))))

(defmacro with-slot-consing ((accessor object &key (when 't)) &body body)
  (alexandria:once-only (object)
    (alexandria:with-gensyms (value)
      `(let ((,value (,accessor ,object)))
         (flet ((compute-the-damn-value ()
                  ,@body))
           (cond
             ((not ,when)
              (compute-the-damn-value))
             ((eq ,value +uncomputed+)
              (setf (,accessor ,object)
                    (compute-the-damn-value)))
             (t
              ,value)))))))

(defmacro with-hash-consing-tables (() &body body)
  `(let ,(loop for name in *table-names*
               collect `(,name (make-hash-table :test 'equal)))
     ,@body))

(defmacro clear-global-tables ()
  "Set up global tables for testing."
  `(setf ,@(loop for name in *table-names*
                 append `(,name (make-hash-table :test 'equal)))))
