(in-package :one-more-re-nightmare)

(defvar *optimize-settings*
  ;; SBCL SPEED doesn't seem to change anything, so we'll just try to
  ;; make the compiler run faster.
  #+sbcl '((speed 0) (safety 0) (compilation-speed 3) (debug 0))
  #-sbcl '((speed 3) (safety 0) (compilation-speed 3) (debug 0)))
