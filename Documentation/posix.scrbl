#lang scribble/base

@require["spec-macros.scrbl" "bibliography.rkt"]
@title{Differences between POSIX and PCRE regexen}

Many regular expression engines implement PCRE-like regular expression
semantics, but one-more-re-nightmare implements POSIX semantics. While
the syntax of POSIX regular expressions is a subset of that of PCRE, the
semantics can differ drastically.

We use one-more-re-nightmare and cl-ppcre to show the differences in
some experiments; the latter is probably the most commonly used
regular expression engine for Common Lisp.

@section{The longest match wins}

When faced with an alternation, PCRE regular expressions prefer the
left-most option, whereas POSIX regular expressions prefer the longest.

@lisp-code{
CL-USER> (one-more-re-nightmare:first-string-match "ant|antler" "antler")
#("antler")
CL-USER> (cl-ppcre:scan-to-strings "ant|antler" "antler")
"ant"
#()
}

Of course, the results are the same when the options are sorted
longest to shortest.

@lisp-code{
CL-USER> (cl-ppcre:scan-to-strings "antler|ant" "antler")
"antler"
#()
}

@section{The longest submatch wins}

The results of the longest match rule produces interesting results
when combined with submatches.

@lisp-code{
CL-USER> (one-more-re-nightmare:first-string-match "«a*»*" "aaa")
#("aaa" "aaa")
CL-USER> (cl-ppcre:scan-to-strings "(a*)*" "aaa")
"aaa"
#("")
}

POSIX prefers matching everything, whereas PCRE prefers matching the empty
string. The behaviour of PCRE can be fixed by requiring at least one iteration,
by @cl{(a+)*}, but this is not necessary with POSIX.

@lisp-code{
CL-USER> (cl-ppcre:scan-to-strings "(a+)*" "aaa")
"aaa"
#("aaa")
}

Another interesting (but more contrived) example is @cl{(a|aa)*}.

@lisp-code{
CL-USER> (one-more-re-nightmare:first-string-match "«a|aa»*" "aaa")
#("aaa" "a")
CL-USER> (one-more-re-nightmare:first-string-match "«a|aa»*" "aaaa")
#("aaaa" "aa")

CL-USER> (cl-ppcre:scan-to-strings "(a|aa)*" "aaa")
"aaa"
#("a")
CL-USER> (cl-ppcre:scan-to-strings "(a|aa)*" "aaaa")
"aaaa"
#("a")
}

Whereas PCRE always takes the left option (@cl{a}), POSIX attempts to
take the right option (@cl{aa}) when possible. The result is that POSIX
produces the submatch @cl{aa} when the string has an even (and
non-zero) number of @cl{a}, and @cl{a} when the string has an odd
number of @cl{a}.

(We think that the "Pike virtual machine", popularised by a series of
articles by Russ Cox @~cite[cox], cannot implement POSIX submatching
semantics, despite its use in some implementations which claim POSIX
compatibility; but we don't have a real proof yet. The basic idea is
that the Pike machine prefers the shortest match that reaches a
particular state, and thus only the shortest submatch is ever saved.
The GNU libc implementation of regular expressions has the same
behaviour as CL-PPCRE on @cl{(a|aa)*} for example.)
