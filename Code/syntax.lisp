(in-package :one-more-re-nightmare)

(esrap:defrule escaped-character
    (and #\\ character)
  (:destructure (backslash char)
    (declare (ignore backslash))
    char))

(esrap:defrule special-character
    (or "(" ")" "¬" "~" "|" "&" "*"))

(esrap:defrule literal
    (* (or escaped-character (not special-character)))
  (:destructure (&rest characters)
    (text (coerce characters 'string))))

(esrap:defrule parens
    (and "(" expressions ")")
  (:destructure (left expression right)
    (declare (ignore left right))
    expression))

(esrap:defrule kleene
    (and expression "*")
  (:destructure (expression star)
    (declare (ignore star))
    (kleene expression)))

(esrap:defrule either
    (and expression "|" expression)
  (:destructure (e1 bar e2)
    (declare (ignore bar))
    (either e1 e2)))

(esrap:defrule both
    (and expression "&" expression)
  (:destructure (e1 bar e2)
    (declare (ignore bar))
    (both e1 e2)))

(esrap:defrule invert
    (and (or "¬" "~") expression*)
  (:destructure (bar expression)
    (declare (ignore bar))
    (invert expression)))

(esrap:defrule expression*
    (or parens invert literal))

(esrap:defrule expression
    (or either
        both
        kleene
        expression*))

(esrap:defrule two-expressions
    (and expression expressions)
  (:destructure (e1 e2)
    (join e1 e2)))

(esrap:defrule expressions
    (or two-expressions expression))

(defun parse-regular-expression (string)
  (esrap:parse 'expressions string))
