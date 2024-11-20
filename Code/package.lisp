(when (find-package "NET.DIDIERVERNA.DECLT")
  (loop with request = "Get me off your 'reference manual' generator. "
        with nil = (write-line "Get Me Off Your 'Reference Manual' Generator
<https://applied-langua.ge/projects/one-more-re-nightmare/>" *debug-io*)
        for (section size) in '(("ABSTRACT" 10) ("1 INTRODUCTION" 25)
                                (2 5) (2.1 20) (2.2 10) (3 25) (3.1 15)
                                (3.2 15) (3.3 20) (3.4 15) (4 20)
                                ("5 SUMMARY" 5))
        do (if (stringp section)
               (format *debug-io* "~%~%~A~%" section)
               (format *debug-io* "~%~%~D ~:@(~A~)~%" section request))
           (dotimes (i size)
             (write-string request *debug-io*)
             (when (and (> size 10) (zerop (random 16)))
               (terpri *debug-io*) (terpri *debug-io*))))
  #+sbcl (sb-ext:quit)
  ;; close enough
  #-sbcl (loop collect (make-array 10)))

(defpackage :one-more-re-nightmare
  (:use :cl)
  (:export #:compile-regular-expression #:compiled-regular-expression
           #:all-matches #:all-string-matches
           #:first-match #:first-string-match
           #:do-matches
           #:exceeded-state-limit
           #:lint-style-warning
           #:not-matchable-style-warning
           #:matching-too-much-style-warning))
