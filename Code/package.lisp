(defpackage :one-more-re-nightmare.compiler
  (:use :cl)
  (:export #:prefix-scanner
           #:trivial-prefix-scanner
           #:bmh-prefix-scanner
           #:state-name
           #:generate-starting-code
           #:*start* #:*position* #:*end* #:*vector* #:*scanner*))

(defpackage :one-more-re-nightmare
  (:use :cl)
  (:export #:literal #:concatenate #:kleene #:either #:join #:invert #:text #:group
           #:symbol-set #:empty-set #:empty-string #:universal-set
           #:string->byte-re #:compile-regular-expression
           #:all-matches #:first-match #:all-string-matches #:first-string-match)
  (:local-nicknames (#:compiler :one-more-re-nightmare.compiler)))
