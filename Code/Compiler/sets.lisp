(in-package :one-more-re-nightmare)

(defclass symbol-set () ())

(defvar *positives*
  (trivial-garbage:make-weak-hash-table :test 'equal :weakness :value))
(defclass positive-symbol-set (symbol-set)
  ((elements :initarg :elements :reader elements))
  (:documentation "A set represented by the elements it contains."))
(defmethod print-object ((set positive-symbol-set) stream)
  (if *print-readably*
      (call-next-method)
      (format stream "{ ~{~a~^, ~} }" (elements set))))
(defmethod make-instance ((class (eql (find-class 'positive-symbol-set)))
                          &rest initargs &key)
  (or (gethash initargs *positives*)
      (setf (gethash initargs *positives*)
            (call-next-method))))

(defvar *negatives*
  (trivial-garbage:make-weak-hash-table :test 'equal :weakness :value))
(defclass negative-symbol-set (symbol-set)
  ((elements :initarg :elements :reader elements))
  (:documentation "A set represented by the elements it does not contain."))
(defmethod print-object ((set negative-symbol-set) stream)
  (if *print-readably*
      (call-next-method)
      (format stream "Σ \\ { ~{~a~^, ~} }" (elements set))))
(defmethod make-instance ((class (eql (find-class 'negative-symbol-set)))
                          &rest initargs &key)
  (or (gethash initargs *negatives*)
      (setf (gethash initargs *negatives*)
            (call-next-method))))

(defun symbol-set (&rest elements)
  (make-instance 'positive-symbol-set :elements elements))

(defun set-equal (set1 set2)
  (null (set-exclusive-or set1 set2)))

(defgeneric set-union (set1 set2)
  (:method ((set1 positive-symbol-set) (set2 positive-symbol-set))
    (make-instance 'positive-symbol-set
                   :elements (union (elements set1) (elements set2))))
  (:method ((set1 positive-symbol-set) (set2 negative-symbol-set))
    (make-instance 'negative-symbol-set
                   :elements (set-difference (elements set2)
                                             (elements set1))))
  (:method ((set1 negative-symbol-set) (set2 positive-symbol-set))
    (make-instance 'negative-symbol-set
                   :elements (set-difference (elements set1)
                                             (elements set2))))
  (:method ((set1 negative-symbol-set) (set2 negative-symbol-set))
    (make-instance 'positive-symbol-set
                   :elements (intersection (elements set1) (elements set2)))))

(defgeneric set-intersection (set1 set2)
  (:method ((set1 positive-symbol-set) (set2 positive-symbol-set))
    (make-instance 'positive-symbol-set
                   :elements (intersection (elements set1) (elements set2))))
  (:method ((set1 positive-symbol-set) (set2 negative-symbol-set))
    (make-instance 'positive-symbol-set
                   :elements (set-difference (elements set1)
                                             (elements set2))))
  (:method ((set1 negative-symbol-set) (set2 positive-symbol-set))
    (make-instance 'positive-symbol-set
                   :elements (set-difference (elements set2)
                                             (elements set1))))
  (:method ((set1 negative-symbol-set) (set2 negative-symbol-set))
    (make-instance 'negative-symbol-set
                   :elements (union (elements set1) (elements set2)))))

(defgeneric set-inverse (set)
  (:method ((set positive-symbol-set))
    (make-instance 'negative-symbol-set :elements (elements set)))
  (:method ((set negative-symbol-set))
    (make-instance 'positive-symbol-set :elements (elements set))))

(defgeneric set-null (set)
  (:method ((set negative-symbol-set)) nil)
  (:method ((set positive-symbol-set))
    (null (elements set))))

(defgeneric symbol-set-equal (set1 set2)
  (:method (s1 s2) nil)
  (:method ((s1 positive-symbol-set) (s2 positive-symbol-set))
    (set-equal (elements s1) (elements s2)))
  (:method ((s1 negative-symbol-set) (s2 negative-symbol-set))
    (set-equal (elements s1) (elements s2))))

(defgeneric make-test-form (set variable)
  (:method ((set positive-symbol-set) variable)
    `(or ,@(loop for element in (elements set)
                 collect `(eql ,variable ',element))))
  (:method ((set negative-symbol-set) variable)
    `(not (or ,@(loop for element in (elements set)
                      collect `(eql ,variable ',element))))))
