# one-more-re-nightmare

one-more-re-nightmare is a regular expression engine that uses the technique
presented in [Regular-expression derivatives revisited](https://www.ccs.neu.edu/home/turon/re-deriv.pdf)
to interpret and compile regular expressions.

Thanks to Gilbert Baumann for suggesting I use derivatives to compile regular
expressions, and my discrete mathematics teachers for properly introducing me
to finite state machines.

## Examples

`INTERPRET-REGULAR-EXPRESSION` and the functions created by 
`COMPILE-REGULAR-EXPRESSION` both take a `vector` argument, and an optional
`start` keyword argument (which defaults to `0`), and return either `NIL` if no
match was found, or the end of the match if a match was found.

```lisp
CL-USER> (use-package :one-more-re-nightmare)
T
CL-USER> (defvar *regexp* (either (join (literal (symbol-set 'a))
                                        (literal (symbol-set 'b)))
                                  (join (literal (symbol-set 'a))
                                        (literal (symbol-set 'c)))))
*REGEXP*

CL-USER> (interpret-regular-expression *regexp* #(a b c))
2
CL-USER> (interpret-regular-expression *regexp* #(b b c))
NIL

CL-USER> (compile-regular-expression *regexp*)
#<FUNCTION (LAMBDA (VECTOR &KEY :START)) {52D1DF1B}>
CL-USER> (funcall * #(a b c))
2
```

## Language

The following expressions can be used:

| Regular expression | o-m-r-n constructor      |
|--------------------|--------------------------|
| ε                  | (empty-string)           |
| ∅                  | (empty-set)              |
| r*                 | (kleene r)               |
| r + s              | (either r s)             |
| r s                | (join r s)               |
| ¬r                 | (invert r)               |
| r & s              | (both r s)               |
| A                  | (literal (symbol-set A)) |


As specified by the paper, these constructors perform some simplification and 
hash-consing, allowing regular expressions to be compared with `eq`.
