(in-package :one-more-re-nightmare)

(defvar *compiler-state*)
(defvar *strategy*)

(defclass compiler-state ()
  ((variable-names :initform (make-hash-table :test 'equal)
                   :reader variable-names)
   (state-names    :initform (make-hash-table :test 'eq)
                   :reader state-names)
   (next-state-name :initform 0
                    :accessor next-state-name)))

(defun find-variable-name (variable)
  (let ((names (variable-names *compiler-state*)))
    (multiple-value-bind (name present?)
        (gethash variable names)
      (if present?
          name
          (setf (gethash variable names)
                (gensym (princ-to-string variable)))))))

(defun find-state-name (state)
  (let ((names (state-names *compiler-state*)))
    (multiple-value-bind (name present?)
        (gethash state names)
      (if present?
          name
          (setf (gethash state names)
                (incf (next-state-name *compiler-state*)))))))

(defun %compile-regular-expression (expression
                                    variable-map
                                    &key strategy)
  (let ((*strategy* strategy)
        (*compiler-state* (make-instance 'compiler-state)))
    (multiple-value-bind (variables declarations body)
        (make-prog-parts expression variable-map)
      (make-complete-form strategy variables declarations body))))
