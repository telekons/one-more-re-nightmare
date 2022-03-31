#lang scribble/base
@require["spec-macros.scrbl"]

@title{Introduction}

one-more-re-nightmare is a new regular expression compiler implemented
in Common Lisp.

@section{Prior work}

The derivative approach was introduced by the late Janusz Brzozowski
in 1964.

The name of the library is due to the song @term{One More Red
Nightmare} by the band King Crimson in 1974.

Ronald Johnston wrote on the APL\3000 compiler in 1979, which produced
specialised code for different array layouts on demand.

Scott Owens, John Reppy and Aaron Turon showed how to produce finite
state machines that are very close to minimal in size, using the derivative
approach. This minimality was achieved by using additional rewrite rules,
allowing the machine generation process to reuse more states, instead of
producing more redundant states.

Jim Newton compiled finite state machines to Common Lisp code, to
implement @term{regular type expressions}.

Gilbert Baumann described how to implement submatching using
derivatives in a currently unpublished paper.
