(in-package :one-more-re-nightmare)

(define-condition no-match () ())

(defvar *environment*)

(defun assert-equivalent (from to)
  (when (and (eql from 'position) (eql to 'position))
    ;; Don't care for POSITION ~ POSITION
    (return-from assert-equivalent))
  (multiple-value-bind (old-to present?)
      (gethash from *environment*)
    (when (and present? (not (equal to old-to)))
      (error 'no-match))
    (setf (gethash from *environment*) to)))

(defun assert-equivalent-sources (from to)
  "Ensure that we don't unify POSITION and a variable, or two different variables."
  (when (eql from to)
    (return-from assert-equivalent-sources))
  (unless (and (listp from) (listp to))
    (error 'no-match))
  (unless (= (first from) (first to))
    (error 'no-match)))

(trivia:defun-match* %similar (from to)
  (((both r1 s1) (both r2 s2))
   (%similar r1 r2)
   (%similar s1 s2))
  (((either r1 s1) (either r2 s2))
   (%similar r1 r2)
   (%similar s1 s2))
  (((join r1 s1) (join r2 s2))
   (%similar r1 r2)
   (%similar s1 s2))
  (((kleene r1) (kleene r2))
   (%similar r1 r2))
  (((invert r1) (invert r2))
   (%similar r1 r2))
  (((literal set1) (literal set2))
   (unless (eq set1 set2)
     (error 'no-match)))
  (((empty-string) (empty-string)))
  (((tag-set set1) (tag-set set2))
   (unless (= (length set1) (length set2))
     (error 'no-match))
   ;; This happens to work nicely as we know that NULLABLE
   ;; will never re-order substitutions in TAG-SETs.
   (loop for (v1 r1 s1) in set1
         for (v2 r2 s2) in set2
         do (unless (eql v1 v2)
              (error 'no-match))
            (assert-equivalent-sources s1 s2)
            (assert-equivalent (list v1 r1)
                               (list v2 r2))))
  (_ (error 'no-match)))

(defun similar (from to)
  (let ((*environment* (make-hash-table :test 'equal)))
    (handler-case
        (values (%similar from to) t)
      (no-match ()
        nil)
      (:no-error (&rest r)
        (declare (ignore r))
        *environment*))))