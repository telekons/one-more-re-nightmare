(in-package :one-more-re-nightmare-tests)

;;;; It's regrind time!

(defvar *start*)
(defvar *end*)

(defun checked-string-ref (string index)
  (assert (and (<= *start* index) (< index *end*)))
  (aref string index))

(defvar *layout*
  (one-more-re-nightmare::make-layout
   :ref 'checked-string-ref))

(defvar *remaining-depth* 4)
(defun random-re ()
  (macrolet ((terminal ()
               ;; A random element of [A-Z].
               '(string (code-char (+ 65 (random 26)))))
             (recurse (control n)
               `(format nil ,control ,@(loop repeat n collect '(random-re)))))
    (if (zerop *remaining-depth*)
        (terminal)
        (let ((*remaining-depth* (1- *remaining-depth*)))
          (case (random 8)
            (0 (terminal))
            (1 (recurse "~a~a" 2))
            (2 (recurse "(~a)" 1))
            (3 (recurse "«~a»" 1))
            (4 (recurse "(~a)|(~a)" 2))
            (5 (recurse "(~a)&(~a)" 2))
            (6 (recurse "(¬~a)" 1))
            (7 (recurse "(~a)*" 1)))))))

(defun random-haystack ()
  (let* ((n (random 80))
         (haystack (make-string n)))
    (dotimes (i n)
      (setf (char haystack i) (code-char (+ 65 (random 26)))))
    haystack))

(defun regrind (n &key (depth 4))
  (let ((success t))
    (lparallel:pdotimes (i n success)
      (let* ((*remaining-depth* depth)
             (one-more-re-nightmare::*make-interpreted-code* t)
             (re (random-re))
             (haystack (random-haystack)))
        (handler-case
            (one-more-re-nightmare:compile-regular-expression
             re
             :layout *layout*)
          (error (e)
            (format t "~&Compiling ~s fails with:~&~a" re e)
            (setf success nil))
          (:no-error (code groups)
            (let ((result (make-array (one-more-re-nightmare::match-vector-size groups)))
                  (*start* 0)
                  (*end* (length haystack)))
              (handler-case
                  (funcall code haystack 0 (length haystack) result
                           (constantly nil))
                (error (e)
                  (format t "~&Matching ~s on the haystack ~s fails with:~&~a" re haystack e)
                  (setf success nil))
                (:no-error (&rest stuff)
                  (declare (ignore stuff))
                  (when (zerop (mod i 100))
                    (write-char #\.)
                    (finish-output)))))))))))
