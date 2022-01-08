(defpackage :one-more-re-nightmare
  (:use :cl)
  (:export #:string->byte-re #:compile-regular-expression
           #:all-matches #:all-string-matches
           #:first-match #:first-string-match
           #:do-matches))
