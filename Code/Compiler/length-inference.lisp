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

(defvar *pointlessly-large-number* most-positive-fixnum)

(defun compute-minimum-lengths (states)
  (let ((work-list '()))
    (flet ((recompute-predecessors-of (state)
             (dolist (pred (predecessors state))
               (pushnew pred work-list))))
      ;; Set the minimum length of every nullable state to be 0, and
      ;; the minimum length of every other state to be pretty large.
      (maphash (lambda (ex state)
                 (setf (minimum-length state)
                       (if (eq (nullable ex) (empty-set))
                           *pointlessly-large-number*
                           0))
                 (recompute-predecessors-of state))
               states)
      ;; Set each minimum length to be one more than the minimum
      ;; length of the successors.
      (loop until (null work-list)
            do (let* ((state (pop work-list))
                      (minimum-successors-length
                        (1+
                         (reduce #'min (state-transitions state)
                                 :key (lambda (transition)
                                        (let ((next-state (transition-next-state transition)))
                                          (if (null next-state)
                                              *pointlessly-large-number*
                                              (minimum-length next-state))))
                                 :initial-value *pointlessly-large-number*))))
                 (when (< minimum-successors-length
                          (minimum-length state))
                   (setf (minimum-length state) minimum-successors-length)
                   (recompute-predecessors-of state))))
      ;; Make sure we did visit all the states.
      (maphash (lambda (ex state)
                 (declare (ignore ex))
                 ;; If any states are stuck at the top value, then set
                 ;; them to zero, so we don't confuse the rest of the
                 ;; compiler.
                 (when (= (minimum-length state) *pointlessly-large-number*)
                   (setf (minimum-length state) 0)))
               states))))
