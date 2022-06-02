#lang racket

(require scriblib/autobib)
(provide generate-bibliography ~cite
         derivatives apl3000 derivatives-reexamined
         rte petalisp cox posix)

(define-cite ~cite citet generate-bibliography)

(define-syntax-rule (define-bib name title author date rest ...)
  (define name
    (make-bib #:title title
              #:author author
              #:date date
              rest ...)))


(define-bib derivatives
  "Derivatives of Regular Expressions"
  "Janus A. Brzozowski"
  1964
  #:url "https://dl.acm.org/doi/10.1145/321239.321249")

(define-bib apl3000
  "The dynamic incremental compiler of APL\\3000"
  "Ronald L. Johnston"
  1979
  #:url "http://www.softwarepreservation.org/projects/apl/Papers/DYNAMICINCREMENTAL")

(define-bib derivatives-reexamined
  "Regular-expression derivatives reexamined"
  (authors "Scott Owens" "John Reppy" "Aaron Turon")
  2009
  #:url "https://www.ccs.neu.edu/home/turon/re-deriv.pdf")

(define-bib rte
  "Type-Checking of Heterogeneous Sequences in Common Lisp"
  (authors "Jim Newton" "Akim Demaille" "Didier Verna")
  2016
  #:url "https://hal.archives-ouvertes.fr/hal-01380792/document")

(define-bib petalisp
  "Petalisp: A Common Lisp Library for Data Parallel Programming"
  "Marco Heisig"
  2018
  #:url "https://dl.acm.org/doi/10.5555/3323215.3323216")

(define-bib cox
  "Regular Expression Matching: the Virtual Machine Approach"
  "Russ Cox"
  2009
  #:url "https://swtch.com/~rsc/regexp/regexp2.html")

(define-bib posix
  "Regular expressions"
  "IEEE"
  2018
  #:url "https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap09.html")
