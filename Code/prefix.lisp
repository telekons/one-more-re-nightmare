(in-package :one-more-re-nightmare)

(trivia:defun-match prefix (re)
  "Identify if the regular expression has a \"prefix\" of group markers we need to take into account."
  ((join r _) (prefix r))
  ((or (empty-string)
       (start-group _)
       (end-group _))
   t)
  (_ nil))
