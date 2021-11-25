(in-package :one-more-re-nightmare)

(defstruct layout
  "A structure representing the type and accessors for a vector of some sort."
  (array-type '(simple-array character 1))
  (ref 'aref)
  (from-number 'code-char)
  (to-number 'char-code)
  (less-or-equal '<=)
  (equal '=))

(defvar *default-layout* (make-layout))
