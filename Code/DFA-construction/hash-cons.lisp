(in-package :one-more-re-nightmare)

;;; A hash-table designed for hash consing. There are two main
;;; deficiencies in the hash tables provided by Common Lisp:
;;; 1. We cannot promise that a key is definitely new and avoid
;;;    comparing keys. We may only "upsert" keys.
;;; 2. The hash function used by SBCL behaves very poorly with
;;;    large substitution lists (as used by TAG-SET).

(defconstant +buckets+ 4096)

(defstruct (hash-cons-table (:constructor make-hash-cons-table (test hash)))
  (buckets (make-array +buckets+ :initial-element '())
   :type (simple-vector #.+buckets+)
   :read-only y)
  (test #'equal :type function :read-only t)
  (hash #'sxhash :type function :read-only t))

(declaim (inline %bucket))
(defun %bucket (hc-table key)
  (mod (the fixnum (funcall (hash-cons-table-hash hc-table) key)) +buckets+))

(defun insert-hc (hc-table key value)
  (push (cons key value)
        (svref (hash-cons-table-buckets hc-table)
               (%bucket hc-table key)))
  value)

(defun lookup-hc (hc-table key)
  (let ((pair (assoc key
                     (svref (hash-cons-table-buckets hc-table) (%bucket hc-table key))
                     :test (hash-cons-table-test hc-table))))
    (if (null pair)
        (values nil nil)
        (values (cdr pair) t))))

(defun tag-set-hash (substitutions)
  (let ((hash 0))
    (flet ((update (x)
             (setf hash (logand most-positive-fixnum (+ (* x 31) hash)))))
      (loop for ((n v) . source) in (first substitutions)
            do (update (sxhash n))
               (update v)
               (trivia:match source
                 ('nil (update 0))
                 ('position (update 1))
                 ((list n v) (update (sxhash n)) (update v)))))
    hash))

(defmethod print-object ((h hash-cons-table) s)
  (print-unreadable-object (h s :type t)
    (format s "~D entries" (reduce #'+ (hash-cons-table-buckets h) :key #'length))))
