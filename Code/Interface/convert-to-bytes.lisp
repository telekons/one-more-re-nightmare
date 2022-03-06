(in-package :one-more-re-nightmare)

#+(or)
(trivia:defun-match string->byte-re (re)
  ((literal set)
   (etypecase set
     (positive-symbol-set
      (reduce #'either
              (elements set)
              :key (lambda (character)
                     (text (babel:string-to-octets
                            (string character))))
              :initial-value (empty-set)))
     (negative-symbol-set
      (reduce #'both
              (elements set)
              :key (lambda (character)
                     (invert
                      (text (babel:string-to-octets
                             (string character)))))
              :initial-value (invert (empty-set))))))
  ((join r s) (join (string->byte-re r)
                    (string->byte-re s)))
  ((either r s) (either (string->byte-re r)
                        (string->byte-re s)))
  ((invert r) (invert (string->byte-re r)))
  ((kleene r) (kleene (string->byte-re r)))
  ((both r s) (both (string->byte-re r)
                    (string->byte-re s)))
  ((type string)
   (string->byte-re (parse-regular-expression re)))
  (_ re))
