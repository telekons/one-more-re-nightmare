(in-package :one-more-re-nightmare)

(defvar *optimize-settings*
  ;; SBCL SPEED doesn't seem to change anything, so we'll just try to
  ;; make the compiler run faster.
  #+sbcl '((speed 0) (safety 0) (compilation-speed 3) (debug 0))
  #-sbcl '((speed 3) (safety 0) (compilation-speed 3) (debug 0)))

(defmacro with-naughty-compiler-switches (() &body body)
  #+sbcl
  `(let ((sb-c::*reoptimize-limit* 1))
     ,@body)
  #-sbcl
  `(progn ,@body))
