(in-package :one-more-re-nightmare)

(stealth-mixin:define-stealth-mixin length-inference-info ()
  state
  ((predecessors :accessor predecessors)
   (minimum-length :accessor minimum-length)))

(defun compute-predecessor-lists (transitions states)
  ;; Clear predecessor lists first.
  (maphash (lambda (ex state)
             (declare (ignore ex))
             (setf (predecessors state) '()))
           states)
  ;; For each transition, add the predecessor to the predecessor list
  ;; of the successor.
  (maphash (lambda (ex transitions)
             (let ((predecessor (gethash ex states)))
               (dolist (transition transitions)
                 (let ((successor (gethash (transition-next-state transition) states)))
                   (unless (null successor)
                     (push predecessor (predecessors successor)))))))
           transitions))

(defvar *pointlessly-large-number* most-positive-fixnum)

(defun compute-minimum-lengths (transitions states)
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
            do (let* ((state       (pop work-list))
                      (transitions (gethash (state-expression state) transitions))
                      (minimum-successors-length
                        (1+
                         (reduce #'min transitions
                                 :key (lambda (transition)
                                        (let ((next-state
                                                (gethash (transition-next-state transition)
                                                         states)))
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
                 (assert (< (minimum-length state) most-positive-fixnum)))
               states))))
