#lang scribble/base
@require["spec-macros.scrbl" "bibliography.rkt"]

@title{Introduction}

one-more-re-nightmare is a new regular expression compiler implemented
in Common Lisp.

The aim is to generate code that will perform as well, if not better,
than hand-written searching code. We believe we have achieved this
aim; as finite state machine generation removes redundant tests that
would not typically be removed by a human programmer, compilation is
performed for specific array types, and the compiler can also make use
of SIMD intrinsics for simple loops. We also intend to produce
excellent analysis, to help with writing regular expressions that do
the "right thing".

@section{Prior work}

The derivative approach was introduced by the late Janusz Brzozowski
in @~cite[derivatives].

The name of the library is due to the song @term{One More Red
Nightmare} by the band King Crimson in 1974.

Ronald Johnston wrote on the APL\3000 compiler in @~cite[apl3000],
which produced specialised code for different array layouts on demand.

Scott Owens, John Reppy and Aaron Turon showed how to produce finite
state machines that are very close to minimal in size in
@~cite[derivatives-reexamined], using the derivative approach. This
minimality was achieved by using additional rewrite rules, allowing
the machine generation process to reuse more states, instead of
producing more redundant states.

Jim Newton compiled finite state machines to Common Lisp code in
@~cite[rte], to implement @term{regular type expressions}.

We were first introduced to using the Common Lisp compiler as a
backend for a @term{just-in-time compiler} by the Petalisp language by
Marco Heisig @~cite[petalisp].

Gilbert Baumann described how to implement submatching using
derivatives in a currently unpublished paper.
