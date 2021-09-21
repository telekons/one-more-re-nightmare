(in-package :one-more-re-nightmare)

(trivia:defun-match re-empty-p (re)
  "Is a regular expression basically an empty string?
This is different to NULLABLE, yes, NULLABLE would accept e.g. a* or anything that is a superset of { \"\" }, but this accepts only the empty string (± tags)."
  ((or (empty-string) (tag-set _)) t)
  ((alpha r s)
   (or (and (re-empty-p s)
            (eq (empty-set) r))
       (re-empty-p r)))
  ((kleene r)
   (re-empty-p r))
  ((or (join r s) (either r s) (both r s))
   (and (re-empty-p r) (re-empty-p s)))
  (_ nil))
  
