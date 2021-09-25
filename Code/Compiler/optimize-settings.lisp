(in-package :one-more-re-nightmare)

(defvar *optimize-settings*
  ;; SBCL SPEED doesn't seem to change anything, so we'll just try to
  ;; make the compiler run faster.
  #+sbcl '((speed 0) (safety 0) (compilation-speed 3) (debug 0))
  ;; Clozure doesn't bother to inline or type infer with high
  ;; COMPILATION-SPEED. Fortunately it's still fast enough.
  #+ccl  '((speed 3) (safety 0) (compilation-speed 0) (debug 0))
  ;; ECL drops in performance when we increase compilation speed, but
  ;; the compiler is not noticeably faster. I blame the C compiler.
  #+ecl  '((speed 3) (safety 0) (compilation-speed 0) (debug 0))
  ;; It seems that we get better performance on ABCL with low
  ;; compilation speed, but there's a lot of noise still.
  #+abcl '((speed 3) (safety 0) (compilation-speed 0) (debug 0))
  #-(or sbcl ccl ecl abcl) '((speed 3) (safety 0) (compilation-speed 3) (debug 0)))

(defmacro with-naughty-compiler-switches (() &body body)
  #+sbcl
  `(let ((sb-c::*reoptimize-limit* 3))
     ,@body)
  #-sbcl
  `(progn ,@body))
