#lang scribble/base
@require["spec-macros.scrbl"]

@title{Interface}

@section{Syntax}

We currently use our own syntax for regular expressions, since the
POSIX syntax does not allow for expressing complements or
intersections of regular expressions.

@centered-block[
@bnf[
@rule["E"
      "E | E" "union"
      "E & E" "intersection"
      "E E" "concatenate"
      "¬E" "complement"
      "~E" "complement"
      "E*" "zero or more repeats"
      "E+" "one or more repeats"
      "E{i}" "repeat"
      "«E»" "submatch"
      "(E)" "change precedence"
      "[c-c]" "character range"
      "[¬c]" "complement character"
      "$" "every character"
      "c" "literal character"
      "<empty string>" ""]
@rule["i" "<integer>" ""]
@rule["c" "<single character>" ""]
]
]

@section{Matching}

Note that one-more-re-nightmare can avoid a cache lookup (involving
acquiring a lock and hash table searching) if the regular expression
is a literal string.  @todo{We could handle the case where the string
is in some constant, e.g. @cl{(defconstant +number+ "[0-9]+")
(all-matches +number+ text)} surely.}

@definitions{
@defun["first-match"]{regular-expression vector @&key start end}

@defun["first-string-match"]{regular-expression vector @&key start end}

Find the first match for @cl{regular-expression} in @cl{vector}
between @cl{start} and @cl{end}.

@cl{first-match} either returns a simple vector, every element of
which is either an index into @cl{vector} or @cl{nil} (when there is
no submatch), or @cl{nil} if there is no match.

@cl{first-string-match} either returns a simple vector, every element
of which is a fresh string or @cl{nil} (when there is no submatch), or
@cl{nil} if there is no match.

}

@definitions{
@defun["all-matches"]{regular-expression vector @&key start end}

@defun["all-string-matches"]{regular-expression vector @&key start end}

Find all matches for @cl{regular-expression} in @cl{vector} between
@cl{start} and @cl{end}.

Both functions return a list of matches; @cl{all-matches} represents
matches as @cl{first-match} does, and @cl{all-string-matches}
represents matches as @cl{first-string-match} does.

}

@definitions{
@defmacro["do-matches"]{((@&rest registers) regular-expression vector @&key start end) @&body body}
}
