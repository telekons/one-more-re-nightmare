# one-more-re-nightmare

one-more-re-nightmare is a regular expression engine that uses the
technique presented in [Regular-expression derivatives
revisited](https://www.ccs.neu.edu/home/turon/re-deriv.pdf) to
interpret and compile regular expressions. We use a few tricks to make
matching quite fast:

- We use a deterministic finite automaton to have O(n) runtime.
- We run the Common Lisp compiler to generate machine code, rather
  than interpreting a DFA or bytecode, or jumping through closures
  (like CL-PPCRE does).
- We generate specialised code for each array type, so everything is
  inlined.
- If you use the `one-more-re-nightmare-simd` system on SBCL 2.1.10 or
  newer with AVX2, we even use vectorised scanning of constant
  prefixes of regular expressions.

Thanks to Gilbert Baumann for suggesting I use derivatives to compile
regular expressions, and then for informing me of how to handle
submatching properly, and my discrete mathematics teachers for
formally introducing me to finite state machines.

## High level interface

The high level interface implicitly caches and compiles regular expressions,
specialised to its input vectors. 

`(all-matches regular-expression vector &key start end)` returns a list of all
`(start end submatch)`s of each match found.

(`submatch` is a vector consisting of either vector indices or `nil`,
consisting of the start of the first group, then the first end, then the second
start, and so on.)

`all-string-matches` takes the same arguments and returns a list of all 
subsequences matching, and a list of vectors of submatch subsequences.

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

| Regular expression | String syntax |
|--------------------|---------------|
| ∑                  | ∑ or $        |
| r*                 | R*            |
| r + s              | R\|S          |
| r r*               | R+            |
| r s                | RS            |
| ¬r                 | ¬R or `R      |
| r & s              | R&S           |
| ABC                | ABC           |
|                    | «R»           |
| r^n                | R{n}          |
| a + b + ... z      | [a-z]         |


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

Note that, by nature of calling the Common Lisp compiler,
one-more-re-nightmare will take longer to compile a regular
expression, so it is better suited for many matching operations with
few expressions. It does cache compiled expressions when using the
high-level interface, so the initial cost may amortize well over many
calls; and constant regular expression strings are compiled at
compile-time, with no runtime overhead whatsoever.

| engine           | SBCL      | Clozure CL | SBCL with AVX2 |
|------------------|-----------|------------|----------------|
| o-m-r-n          | 0.57ms    | 2.93ms     | 0.18ms         |
| compilation time | 4.65ms    | 3.76ms     | 6.82ms         |
| cl-ppcre         | 22.8ms    | 45.3ms     | 22.8ms         |
| break even after | 209kchars | 88.7kchars | 301.5kchars    |
