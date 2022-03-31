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

Please see [the reference
book](https://applied-langua.ge/projects/one-more-re-nightmare/) for
how to use one-more-re-nightmare, or [an
article](https://applied-langua.ge/posts/omrn-compiler.html) on the
history and theory involved.

While the syntax is admittedly wonky (but somewhat more like how
regular expressions are presented in papers), one-more-re-nightmare
makes its best effort to implement POSIX semantics for matching (as
described in the specification for [how `regcomp`
works](https://pubs.opengroup.org/onlinepubs/9699919799/functions/regexec.html)
and [regular expression
definitions](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap09.html)). Any
behaviour contrary to POSIX is a bug.

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

| engine           | SBCL      | Clozure CL | SBCL with AVX2 | ditto, SIMPLE-BASE-STRING |
|------------------|-----------|------------|----------------|---------------------------|
| o-m-r-n          | 0.57ms    | 2.93ms     | 0.18ms         | 55µs                      |
| compilation time | 4.65ms    | 3.76ms     | 6.82ms         | 6.43ms                    |
| cl-ppcre         | 22.8ms    | 45.3ms     | 22.8ms         | 21.6ms                    |
| break even after | 209kchars | 88.7kchars | 301kchars      | 305kchars                 |
