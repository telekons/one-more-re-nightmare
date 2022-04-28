#lang scribble/base
@require["spec-macros.scrbl" scribble-math/dollar]

@title{Linting and warnings}

one-more-re-nightmare can produce warnings at compile-time for various
mistakes when writing regular expressions. The compiler produces a
finite state machine, which involves traversing every execution path
of the machine, so it can perform analysis with no false positives or
negatives.

Linting occurs when regular expressions are provided as literal
strings in the source code. All @term{style warnings} generated are of the
type @cl{lint-style-warning}.

@section{Unreachability}

@definitions{
@define-condition["not-matchable-style-warning" "style-warning"]
}

The following issues generate @term{style warnings} at
compile-time, of type @cl{not-matchable-style-warning}. They do not
indicate that something will go wrong at run-time, but their behaviour
is rarely desirable.

@subsection{"This expression is impossible to match."}

@definition-section["Explanation"]{

The expression will never match any expressions; it is equivalent to
the empty set.

}

@definition-section["Examples"]{

@cl{a&b}: There are no characters that are simultaneously @cl{a} and @cl{b}.

@cl{¬(a$|$b)&¬(¬(a$)&¬($b))}: There are no strings that match
@cl{¬(a$|$b)} but not @cl{¬(a$)&¬($b)}.  In other words, the linter is
used to prove @${\overline{a \lor b} \Rightarrow \overline a \land
\overline b}. While it is a fun idea, we don't recommend using the
linter to check equivalence of Boolean expressions.

}

@subsection{"The <n-th> group in this expression is impossible to match."}

@definition-section["Explanation"]{

A submatch in the expression will never match any expressions; it may
either correspond to the empty set, or is "shadowed" by an alternate
expression.

}

@definition-section["Examples"]{

@cl{a|«a»} generates the warning "The first group in this expression
is impossible to match.". The only string that the expression can
match is @cl{a}, and the left-hand side of the @cl{|} operator takes
precedence with POSIX semantics, so the right hand side can never match.

@cl{a|«b&c»} generates the same warning. There are no characters that
are simultaneously @cl{b} and @cl{c}.

}

@section{Matching too much}

@definitions{
@define-condition["matching-too-much-style-warning" "style-warning"]
}

Some regular expressions may match at every position, which is usually
a sign of a mistake, as one usually wants to extract something from a
string, and not everything. The following issues generate
@term{style warnings} at compile-time, of type
@cl{matching-too-much-style-warning}.

@subsection{"This expression matches the empty string at every position."}

@definition-section["Explanation"]{

The expression will match at every position, and most matches will
have zero length. Often some @cl{*} repetition needs to be replaced with
some @cl{+} repetition, to ensure matches contain at least one character.

}

@definition-section["Examples"]{

The following code will produce too many matches:

@lisp-code{
(defun numbers (string)
  (one-more-re-nightmare:all-string-matches "[0-9]*" string))
(numbers "Phone: 6323003")
;; => (#("") #("") #("") #("") #("") #("") #("") #("6323003") #(""))
}

one-more-re-nightmare generates this warning when the @cl{numbers}
function is submitted. One solution is to replace the @cl{*} repetition
with @cl{+} repetition.

@lisp-code{
(defun numbers (string)
  (one-more-re-nightmare:all-string-matches "[0-9]+" string))
(numbers "Phone: 6323003")
;; => (#("6323003"))
}

}

@subsection{"This expression will only ever match the empty string at
every position."}

@definition-section["Explanation"]{

The expression will only match at every position, and all matches will
have zero length.

}

@definition-section["Examples"]{

Using the empty string as a regular expression generates this warning.
Other regular expressions which are not just the empty string can still
generate this warning; @cl{|b&c} will generate this warning, as the
regular expression still can only match the empty string.

}

@section{Syntax errors}

Syntax errors can also be caught at compile-time, signalling full
warnings, as function with invalid syntax will always fail at
run-time.

@definition-section["Examples"]{

@cl{(} generates a parsing error. The open-parenthesis should be
matched with a closing @cl{)}.

}

@section{Type errors}

Type errors can be caught at compile-time, signalling full warnings,
as functions with type errors will always fail at run-time.

@subsection{"This regular expression only produces <m> registers, but
<n> variables were provided."}

@definition-section["Explanation"]{

Too many register variables were provided for the regular expression
provided to @cl{do-matches}.

}

@definition-section["Examples"]{

@cl{(one-more-re-nightmare:do-matches ((start end s1 e1) "abcde" x)
(print (list s1 e1)))} generates the warning "This regular expression
only produces two registers, but four variables were provided." There
are no submatches in @cl{abcde}, but the @cl{do-matches} form was
provided the variable names @cl{s1} and @cl{e1} for a submatch.

}

@subsection{SBCL reports a type conflict}

@definition-section["Explanation"]{

one-more-re-nightmare provides specific types to SBCL for regular
expressions provided as string literals. The SBCL compiler can use
these types to detect errors in code that uses the results produced by
one-more-re-nightmare.

Specifically, one-more-re-nightmare provides the return type
@code-template{(or null (simple-vector @var{@${2(n+1)}}))} for a call
to @cl{first-match} with a regular expression with @${n} submatches.
one-more-re-nightmare provides the type @cl{alexandria:array-index}
for the first two register variables, and the type @cl{(or null
alexandria:array-index)} for the remaining variables for @cl{do-matches}.

}

@definition-section["Examples"]{

@cl{(svref (first-match "abc" "abc") 2)} generates the warning
"Derived type (INTEGER 2 2) is not a suitable index for (SIMPLE-VECTOR
2)."

@cl{(do-matches ((s) "ab|ac" "ab") (print (symbol-name s)))} generates
the warning "Derived type of ... is (VALUES (MOD ...) &OPTIONAL)
conflicting with its asserted type SYMBOL." The variable @cl{s} will
always be bound to an index, and never @cl{nil}, because the first two
registers designate the bounds of the entire match.

}
