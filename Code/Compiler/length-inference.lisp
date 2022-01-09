(in-package :one-more-re-nightmare)

(stealth-mixin:define-stealth-mixin length-inference-info ()
  state
  ((predecessors :accessor predecessors)
   (minimum-length :accessor minimum-length)))

(defun compute-predecessor-lists (states)
  ;; Clear predecessor lists first.
  (maphash (lambda (ex state)
             (declare (ignore ex))
             (setf (predecessors state) '()))
           states)
  ;; For each transition, add the predecessor to the predecessor list
  ;; of the successor.
  (maphash (lambda (ex predecessor)
             (declare (ignore ex))
             (dolist (transition (state-transitions predecessor))
               (let ((successor (transition-next-state transition)))
                 (unless (null successor)
                   (push predecessor (predecessors successor))))))
           states))

;;; The following function implements Kildall's algorithm for
;;; optimisation, as described in
;;; <https://calhoun.nps.edu/bitstream/handle/10945/42162/Kildall_A_unified_approach_1973.pdf>.
;;; The end result of this analysis is that we can replace several
;;; bounds checks, each checking for one more character, with one
;;; check that checks for multiple characters.

;;; To compute the minimum lengths required to match from each state,
;;; this algorithm first optimistically makes far too high guesses
;;; about minimum lengths: if a state is not accepting, it guesses
;;; that the state will never reach an accepting state, and thus has
;;; an infinite minimum length; else, it correctly determines that the
;;; minimum length is 0 when the state is accepting. We then propagate
;;; lengths. A state takes one more character to match than its
;;; successors (the "transfer" function), and a set of successors
;;; states has a minimum length that is the minimum of the minimum
;;; length of each successor (the "confluence" function). We treat
;;; states that never reach an accepting state as having infinite
;;; minimum length, so our implementation has to handle some
;;; additional cases:
;;;     min(infinity, X) = min(X, infinity) = X for all X
;;; and
;;;     infinity + 1 = infinity.
;;; Minimum lengths are repeatedly propagated until we don't actually
;;; lower anything by propagation. Then we have a correct result.

(defun compute-minimum-lengths (states)
  (let ((work-list '()))
    (flet ((recompute-predecessors-of (state)
             (dolist (pred (predecessors state))
               (pushnew pred work-list)))
           (confluence (x y)
             (cond
               ((eql x :infinity) y)
               ((eql y :infinity) x)
               (t (min x y))))
           (transfer (x)
             (if (eql x :infinity)
                 :infinity
                 (1+ x)))
           (lower-p (new old)
             (if (eql old :infinity)
                 (not (eql new :infinity))
                 (< new old))))
      ;; Set the minimum length of every nullable state to be 0, and
      ;; the minimum length of every other state to be pretty large.
      (maphash (lambda (ex state)
                 (setf (minimum-length state)
                       (if (eq (nullable ex) (empty-set))
                           :infinity
                           0))
                 (recompute-predecessors-of state))
               states)
      ;; Set each minimum length to be one more than the minimum
      ;; length of the successors.
      (loop until (null work-list)
            do (let* ((state (pop work-list))
                      (minimum-successors-length
                        (transfer
                         (reduce #'confluence
                                 (state-transitions state)
                                 :key (lambda (transition)
                                        (minimum-length (transition-next-state transition)))
                                 :initial-value :infinity))))
                 (when (lower-p minimum-successors-length (minimum-length state))
                   (setf (minimum-length state) minimum-successors-length)
                   (recompute-predecessors-of state))))
      ;; Make sure we did visit all the states.
      (maphash (lambda (ex state)
                 (declare (ignore ex))
                 ;; If any states are stuck at the top value, then set
                 ;; them to zero, so we don't confuse the rest of the
                 ;; compiler.
                 (when (eql (minimum-length state) :infinity)
                   (setf (minimum-length state) 0)))
               states))))
