# one-more-re-nightmare

one-more-re-nightmare is a regular expression engine that uses the technique
presented in [Regular-expression derivatives revisited](https://www.ccs.neu.edu/home/turon/re-deriv.pdf)
to interpret and compile regular expressions. And I mean *compile* regular
expressions. To actual machine code (well, Common Lisp, it goes to machine 
code if you use a Lisp implementation that generates machine code somehow.)
It's probably quite fast.

Thanks to Gilbert Baumann for suggesting I use derivatives to compile regular
expressions, and my discrete mathematics teachers for properly introducing me
to finite state machines.

## High level interface

The high level interface implicitly caches and compiles regular expressions,
specialised to its input vectors. 

`(all-matches regular-expression vector &key start end)` returns a list of all
`(start end)`s of each match found.

`all-string-matches` takes the same arguments and returns a list of all 
subsequences matching.

`(first-match regular-expression vector &key start end)` returns the start and 
end of the first match as multiple values, or two `NIL`s if no match was found.

`first-match` takes the same arguments and returns a subsequence of the first 
match or `NIL`.

## Low level interface

`(compile-regular-expression regular-expression &key vector-type)` compiles a
regular expression into a function that takes arguments 
`(vector start end continuation)`, calling `continuation` with each start and
end of each match in the `vector` between `start` and `end`.

## Language

The following expressions can be used:

| Regular expression | o-m-r-n constructor | string syntax |
|--------------------|---------------------|---------------|
| ε                  | (empty-string)      |               |
| ∅                  | (empty-set)         |               |
| ∑                  | (universal-set)     | ∑ or $        |
| r*                 | (kleene r)          | R*            |
| r + s              | (either r s)        | R\|S          |
|                    | (join r (kleene r)) | R+            |
| r s                | (join r s)          | RS            |
| ¬r                 | (invert r)          | ¬R or `R      |
| r & s              | (both r s)          | R&S           |
| ABC                | (text "ABC")        | ABC           |
|                    | (group r n)         | «R»           |


As specified by the paper, these constructors perform some simplification and 
hash-consing, allowing regular expressions to be compared with `eq`.

`¬R` binds tighter than `R*`, so `¬R*` parses as `(kleene (invert R))`.
`R|S` also binds tighter than `R&S`, so `A|B&C|D` parses as 
`(both (join A B) (join C D))`.

## A lousy benchmark

```lisp
CL-USER> (let ((s (make-string 1000000 :initial-element #\a)))
           (setf (aref s 333333) #\b)
           (setf (aref s 555555) #\c)
           (the-cost-of-nothing:bench
            (all-string-matches "ab|ac" s)))

CL-USER> (let ((s (make-string 1000000 :initial-element #\a)))
           (setf (aref s 333333) #\b)
           (setf (aref s 555555) #\c)
           (the-cost-of-nothing:bench
            (cl-ppcre:all-matches-as-strings "ab|ac" s)))
```

| engine   | SBCL   | Clozure CL | ECL    | ABCL   |
|----------|--------|------------|--------|--------|
| o-m-r-n  | 2.59ms | 3.78ms     | 1.83ms | 5.83ms |
| cl-ppcre | 22.8ms | 40.1ms     | 225ms  | 239ms  |
