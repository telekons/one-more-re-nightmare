(in-package :one-more-re-nightmare)

(defvar *next-group*)
(defun next-group ()
  (prog1 *next-group*
    (incf *next-group* 2)))

(esrap:defrule escaped-character
    (and #\\ character)
  (:destructure (backslash char)
    (declare (ignore backslash))
    char))

(esrap:defrule special-character
    (or "(" ")" "«" "»" "¬" "~" "|" "&" "*" "∑" "$" "+"))

#|
«expression*»
(expression*)
under-both   & under-both
under-either | under-either
|#

(esrap:defrule literal
    (* (or escaped-character (not special-character)))
  (:destructure (&rest characters)
    (text (coerce characters 'string))))

(esrap:defrule parens
    (and "(" expressions ")")
  (:destructure (left expression right)
    (declare (ignore left right))
    expression))

(esrap:defrule match-group
    (and "«" expressions "»")
  (:around ()
    (let ((group-number (next-group)))
      (destructuring-bind (left expressions right)
          (esrap:call-transform)
        (declare (ignore left right))
        (group expressions group-number)))))

(esrap:defrule kleene
    (and expression "*")
  (:destructure (expression star)
    (declare (ignore star))
    (kleene expression)))

(esrap:defrule plus
    (and expression "+")
  (:destructure (expression plus)
    (declare (ignore plus))
    (join expression (kleene expression))))

(esrap:defrule either
    (and under-either "|" under-either)
  (:destructure (e1 bar e2)
    (declare (ignore bar))
    (either e1 e2)))

(esrap:defrule both
    (and under-both "&" under-both)
  (:destructure (e1 bar e2)
    (declare (ignore bar))
    (both e1 e2)))

(esrap:defrule invert
    (and (or "¬" "~") expression*)
  (:destructure (bar expression)
    (declare (ignore bar))
    (invert expression)))

(esrap:defrule universal-set
    (or "∑" "$")
  (:constant (universal-set)))

(esrap:defrule expression*
    (or match-group parens invert universal-set literal))

(esrap:defrule under-either
    (or plus kleene expression*))

(esrap:defrule under-both
    (or either under-either))

(esrap:defrule expression
    (or both under-both))

(esrap:defrule two-expressions
    (and expression expressions)
  (:destructure (e1 e2)
    (join e1 e2)))

(esrap:defrule expressions
    (or two-expressions expression))

(defun parse-regular-expression (string)
  (let ((*next-group* 1))
    (esrap:parse 'expressions string)))
