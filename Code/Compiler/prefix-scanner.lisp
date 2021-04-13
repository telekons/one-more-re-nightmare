(in-package :one-more-re-nightmare)

(defclass compiler:prefix-scanner ()
  ())

(defclass compiler:trivial-prefix-scanner (compiler:prefix-scanner)
  ())

(defvar compiler:*scanner*)

(defvar compiler:*start*    'this-start)
(defvar compiler:*position* 'position)
(defvar compiler:*end*      'end)
(defvar compiler:*vector*   'vector)

(defvar *compiler-state*)
(defvar *dfa-roots*)

(defun compiler:state-name (regular-expression)
  (pushnew regular-expression *dfa-roots*)
  (re-name *compiler-state* regular-expression))

(defgeneric compiler:generate-starting-code
    (scanner regular-expression aref-generator fail)
  (:documentation "Generate code to use to enter DFA matching, and a list of variables with initial values (to be LET* outside the match loop generated).")
  (:method ((scanner compiler:trivial-prefix-scanner)
            regular-expression aref-generator fail)
    (values
     `(progn
        (setf ,compiler:*position* ,compiler:*start*)
        (go ,(compiler:state-name regular-expression)))
     '())))
