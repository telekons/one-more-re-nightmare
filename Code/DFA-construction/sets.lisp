(in-package :one-more-re-nightmare)

;;; Gilbert Baumann's isum.lisp

;; MAKE-TEST-FORM

;; To support large character sets, we need an implemention of a set of
;; characters. Traditional scanner generators would at some place just
;; enumerate the alphabet \Sigma, which is not feasible with large character
;; sets like Unicode.

;; We handle all transitions in the automaton as a set of of the codes of
;; characters, expressed by an ISUM. The representation of such a set is
;; best defined by the ISUM-MEMBER function, but here is an overview to get
;; the idea:

;;     ()           is the empty set
;;     (a b)        is the set [a, b)
;;     (a b c d)    is the set [a, b) u [c, d)
;;     (nil)        is everything
;;     (nil a b)    is everything but [a, b)

;; An ISUM is a sequence of stricly monotonic increasing integers. The idea
;; is that when you sweep a pointer over the list at each element found the
;; membership in the set changes. Like (1 10 12 15). You start outside the
;; set, find 1 and say "above or equal 1 is in the set" and then find 10 and
;; say "above or equal 10 is not in the set" and so on. This way it is very
;; easy to implement Boolean operations on sets.

(alexandria:define-constant +empty-set+ '() :test 'equal)
(alexandria:define-constant +universal-set+ '(nil) :test 'equal)

(defun singleton-set (x)
  "Returns the ISUM, that contains only /x/."
  (list x (1+ x)))

(defun symbol-range (from below)
  "Returns the ISUM, that contains every code point that is in [from, below)"
  (list from below))

;;; Boolean operation on ISUMs

(defmacro isum-op (op A B)
  "Combine the sets A and B by the Boolean operator op, which should be a
valid argument to the BOOLE function. An integer x is member of the
resulting set iff
     (logbitp 0 (boole op (if (isum-member x A) 1 0) (if (isum-member x B) 1 0)))
 is non-NIL. That way e.g. boole-ior denotes the union."
  `(let ((A ,A)
         (B ,B))
     (let* ((Ain 0)
            (Bin 0)
            (Cin 0)
            (s nil)
            (res (cons nil nil))
            (resf res))
       ;; Get rid of an initial NIL, which indicates a complemented set.
       (when (and A (null (car A)))
         (pop A) (setq Ain (- 1 Ain)))
       (when (and B (null (car B)))
         (pop B) (setq Bin (- 1 Bin)))
       ;; Now traverse A and B in parallel and generate the resulting sequence.
       (loop
         (when (/= Cin (ldb (byte 1 0) (boole ,op Ain Bin)))
           (setf resf (setf (cdr resf) (cons s nil)))
           (setf Cin (- 1 Cin)))
         (cond ((null A)
                (cond ((null B)
                       (return))
                      (t
                       (setq s (pop B))
                       (setq Bin (- 1 Bin)))))
               ((null B)
                (setq s (pop A)) (setq Ain (- 1 Ain)))
               ((< (car A) (car B))
                (setq s (pop A)) (setq Ain (- 1 Ain)))
               ((< (car B) (car A))
                (setq s (pop B)) (setq Bin (- 1 Bin)))
               (t
                (setq s (pop A)) (setq Ain (- 1 Ain))
                (pop B) (setq Bin (- 1 Bin)))))
       (cdr res))))

;; Now we could define interesting set operations in terms of ISUM-OP.

(defun set-union (a b)             (isum-op boole-ior a b))
(defun set-intersection (a b)      (isum-op boole-and a b))
(defun symbol-set-difference (a b) (isum-op boole-andc2 a b))
(defun set-inverse (a)             (isum-op boole-c1 a nil))
(defun set-null (isum)             (null isum))

(defun symbol-set (&rest symbols)
  (reduce #'set-union symbols :key #'singleton-set :initial-value +empty-set+))

(trivia:defpattern single-isum-case (a next)
  (alexandria:with-gensyms (succ)
    `(trivia:guard (list* ,a ,succ ,next)
                   (= ,succ (1+ ,a)))))

(defun fold-or (form next)
  "Manually constant fold out (OR A NIL) to A. The compiler can do this, but generated code looks nicer with folding."
  (if (eql next 'nil)
      form
      `(or ,form ,next)))

(defun make-test-form (isum variable less-or-equal equal)
  (trivia:ematch isum
    ('() 'nil)
    ((list* nil next)
     (trivia:match (make-test-form next variable less-or-equal equal)
       ('nil 't)
       (form `(not ,form))))
    ((single-isum-case a next)
     (fold-or `(,equal ,a ,variable)
              (make-test-form next variable less-or-equal equal)))
    ((list* low high next)
     (fold-or `(,less-or-equal ,low ,variable ,(1- high))
              (make-test-form next variable less-or-equal equal)))))

(defun print-isum (isum stream)
  (labels ((print-union (rest)
             (trivia:ematch rest
               ('())
               ((single-isum-case a next)
                (write-char (code-char a) stream)
                (print-union next))
               ((list* a b next)
                (format stream "~c-~c" (code-char a) (code-char (1- b)))
                (print-union next)))))
    (trivia:ematch isum
      ('() (write-string "ø" stream))
      ((single-isum-case a 'nil)
       (write-char (code-char a) stream))
      ((list* 'nil (single-isum-case a 'nil))
       (format stream "[¬~c]" (code-char a)))
      ((list* nil rest)
       (write-string "[¬" stream)
       (print-union rest)
       (write-string "]" stream))
      (_
       (write-string "[" stream)
       (print-union isum)
       (write-string "]" stream)))))
