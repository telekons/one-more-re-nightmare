(in-package :one-more-re-nightmare)

(defstruct (layout (:constructor %make-layout))
  "A structure representing the type and accessors for a vector of some sort."
  (array-type '(simple-array character 1))
  (ref 'aref)
  (from-number 'code-char)
  (to-number 'char-code)
  (less-or-equal '<=)
  (equal '=))

(defun make-layout (array-type)
  (if #+(and sbcl x86-64) (equal array-type '(simple-array character 1))
      #-(and sbcl x86-64) nil
      (%make-layout
       :array-type array-type
       :ref '%string-ref
       :to-number 'identity)
      (%make-layout
       :array-type array-type)))
