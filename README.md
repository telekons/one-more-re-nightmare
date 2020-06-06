# one-more-re-nightmare

one-more-re-nightmare is a regular expression engine that uses the technique
presented in [Regular-expression derivatives revisited](https://www.ccs.neu.edu/home/turon/re-deriv.pdf)
to interpret and compile regular expressions.

Thanks to Gilbert Baumann for suggesting I use derivatives to compile regular
expressions, and my discrete mathematics teachers for properly introducing me
to finite state machines.

## High level interface

`(all-matches regular-expression vector &key start end)` returns a list of all
`(start end)`s of each match found.

`all-string-matches` takes the same arguments and returns a list of all 
subsequences matching.

`(first-match regular-expression vector &key start end)` returns the start and 
end of the first match as multiple values, or two `NIL`s if no match was found.

`first-match` takes the same arguments and returns a subsequence of the first 
match or `NIL`.

## Low level interface

`(interpret-regular-expression regular-expression vector &key (start 0))` 
tries to find a match starting from `start`, returning the end position or NIL.

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
```

`(compile-regular-expression regular-expression &key vector-type)` compiles a
regular expression into a function that takes arguments 
`(vector start end continuation)`, calling `continuation` with each start and
end of each match in the `vector` between `start` and `end`.

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
