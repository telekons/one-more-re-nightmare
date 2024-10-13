(in-package :one-more-re-nightmare)

;;;; Character sums
;;; Character sums (csums) represent a set of characters as
;;; a combination of ranges and "symbolic" classes (such as
;;; [:alpha:], [:digit:], etc). Their implementation is somewhat
;;; similar to Gilbert Baumann's "isum" integer sums, used
;;; in clex2 and earlier versions of one-more-re-nightmare.

;;; Class sets
;; A class set is an element of ℙ(ℙ(classes)) i.e. a set of sets
;; of character classes that are part of a range.

;; This gets normalised to P on SBCL, but it looks pretty.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ℙ (x) (expt 2 x)))
(alexandria:define-constant +classes+
    '((:alpha alpha-char-p) (:digit digit-char-p) (:lower lower-case-p) (:upper upper-case-p))
  :test 'equal)
(defconstant +empty-class-set+ 0)
(defconstant +class-set-bits+ (ℙ (length +classes+)))
(defconstant +universal-class-set+ (1- (ℙ (ℙ (length +classes+)))))
(defun class-set-complement (c) (logxor #xFFFF c))

;;; Character sets
;; A character set is a union of intersections of character ranges
;; and class sets. A character set has the form ((class-set start end) ...)
;; with the first start fixed to 0 and the last end fixed to *CODE-LIMIT*.

(defvar *code-limit* char-code-limit)
(define-symbol-macro +empty-set+ (list (list +empty-class-set+ 0 *code-limit*)))
(define-symbol-macro +universal-set+ (list (list +universal-class-set+ 0 *code-limit*)))
(defun range (start limit)
  "The character set for [START, LIMIT)"
  (list (list +empty-class-set+ 0 start)
        (list +universal-class-set+ start limit)
        (list +empty-class-set+ limit *code-limit*)))
(defun singleton-set (x) (range x (1+ x)))
(defun class-set (class)
  (let ((p (position class +classes+ :key #'first)))
    (when (null p) (error "No class named ~S" class))
    (loop for i below +class-set-bits+
          when (logbitp p i)
            sum (ash 1 i) into class-set
          finally (return (list (list class-set 0 *code-limit*))))))
(defun remove-empty-ranges (csum)
  (remove 0 csum :key #'first))

(defun print-csum (csum stream)
  #+sbcl (declare (sb-ext:muffle-conditions type-i:failed-type-inference))
  (labels ((range (start end)
             (if (= (1- end) start)
                 (string (code-char start))
                 (format nil "~C-~C" (code-char start) (code-char (1- end))))))
    (trivia:match (remove-empty-ranges csum)
      ('() (write-string "[]" stream))
      ((equal +universal-set+) (write-string "Σ" stream))
      ((list (list (= +universal-class-set+) start end))
       (if (= (1- end) start)
         (write-char (code-char start) stream)
         (format stream "[~C-~C]" (code-char start) (code-char (1- end)))))
      (pos-parts
       (trivia:match (remove-empty-ranges (csum-complement csum))
         ((list (list (= +universal-class-set+) start end))
          (format stream "[¬~A]" (range start end)))
         (neg-parts
          (write-char #\[ stream)
          (multiple-value-bind (negative? parts)
              (if (> (length pos-parts) (length neg-parts))
                  (values t neg-parts)
                  (values nil pos-parts))
            (when negative? (write-char #\¬ stream))
            (loop for (classes start end) in parts
                  if (= classes +universal-class-set+)
                    do (write-string (range start end) stream)
                  else
                    do (write (list classes start end) :stream stream)))
          (write-char #\] stream)))))))

;;; Operations on character sets

(defun coalesce-csum (cset)
  "Coalesce adjacent ranges with the same class set in a character set."
  (loop until (null cset)
        collect (let* ((f (first cset))
                       (l (member (first f) (rest cset) :key #'first :test #'/=)))
                  (setf cset l)
                  (if (null l)
                      (list (first f) (second f) *code-limit*)
                      (list (first f) (second f) (second (first l)))))))

;; A set table is a list of lists of values, and a list of ranges.
;; We ensure that ranges line up by only storing the ranges
;; once.
(defun align-csums (csets)
  "Align the ranges in a list of character sets, returning a list of lists of values, and a list of ranges."
  (labels ((align (csets values ranges start)
             (if (null (first csets))
                 (values (reverse values) (reverse ranges))
                 (let ((end (reduce #'min csets :key (lambda (c) (third (first c))))))
                   ;; Take a step.
                   (align
                    (loop for c in csets
                          collect (if (= (third (first c)) end) (rest c) c))
                    (cons (loop for c in csets collect (first (first c))) values)
                    (cons (list start end) ranges)
                    end)))))
    (align csets '() '() 0)))

(defmacro define-csum-op (name class-op arguments)
  `(defun ,name ,arguments
     (multiple-value-bind (values ranges)
         (align-csums (list ,@arguments))
       (coalesce-csum
        (mapcar (lambda (v r) (cons (apply #',class-op v) r))
                values ranges)))))

(define-csum-op csum-union logior (a b))
(define-csum-op csum-intersection logand (a b))
(define-csum-op csum-complement class-set-complement (a))
(define-csum-op csum-difference logandc2 (a b))
(defun csum-null-p (csum) (equal csum +empty-set+))

;;; Character set dispatch
;; This could have element type (UNSIGNED-BYTE 4) but that'd take
;; more effort to decode; so we go with bytes.
;; Rhetorical question: Are there any other ways to compress this
;; table?
(alexandria:define-constant +character-class-table+
    (let ((table
            (make-array char-code-limit
                        :element-type '(unsigned-byte 8)
                        :initial-element 0)))
      (dotimes (i char-code-limit table)
        (let ((character (code-char i)))
          (unless (null character)
            (loop for (nil predicate) in +classes+
                  for x = 1 then (ash x 1)
                  do (when (funcall predicate (code-char i))
                       (setf (aref table i) (logior x (aref table i)))))))))
  :test 'equalp)
(declaim (inline lookup-class))
(defun lookup-class (code)
  (aref +character-class-table+ code))

(defmacro csum-case (var less-than equal &body cases)
  (labels ((dispatch-classes (values)
             (if (alexandria:length= 1 values)
                 `(progn ,@(cdar values))
                 (alexandria:with-gensyms (result)
                   `(let ((,result (lookup-class ,var)))
                      (cond
                        ,@(loop for (class-set . body) in values
                                collect `(,(if (= +universal-class-set+ class-set)
                                               't
                                               `(logbitp ,result ,class-set))
                                          ,@body)))))))
           (singleton-p (range) (= (1+ (first range)) (second range)))
           (middle (list) (butlast (rest list)))
           (dispatch-csums (values ranges)
             (cond
               ((alexandria:length= 1 ranges)
                ;; There's only one more range, so dispatch on classes.
                (dispatch-classes (first values)))
               ;; Detect singleton sets to use = on, e.g. [^ab], a and b.
               ((and (equal (first values) (first (last values)))
                     (every #'singleton-p (middle ranges)))
                `(cond
                   ,@(loop for r in (middle ranges)
                           for v in (middle values)
                           collect `((,equal ,var ,(first r)) ,(dispatch-classes v)))
                   (t ,(dispatch-classes (first values)))))
               ;; Bisect and continue dispatching.
               (t
                (let* ((mid (floor (length values) 2)))
                  `(if (,less-than ,var ,(first (nth mid ranges)))
                       ,(dispatch-csums (subseq values 0 mid)
                                        (subseq ranges 0 mid))
                       ,(dispatch-csums (subseq values mid)
                                        (subseq ranges mid))))))))
    (multiple-value-bind (values ranges)
        (align-csums
         (loop for (csum . body) in cases
               collect (loop for (cl s e) in csum
                             collect `((,cl . ,body) ,s ,e))))
      ;; Remove unreachable values from the set table.
      (let ((values (mapcar #'remove-empty-ranges values)))
        (dispatch-csums values ranges)))))

(defun csum-has-classes-p (csum)
  "Does a character sum use any non-trivial character classes?"
  (loop for (c s e) in csum
        thereis (and (/= c +empty-class-set+) (/= c +universal-class-set+))))

(defun make-test-form (csum variable)
  "Compute a form which tests if VARIABLE is an element of CSUM, using OR, <= and ="
  (cond
    ((equal csum +empty-set+) 'nil)
    ((equal csum +universal-set+) 't)
    (t
     `(or ,@(loop for (c s e) in csum
                  unless (= c +empty-class-set+)
                    do (assert (= c +universal-class-set+))
                    and collect (if (= (1+ s) e)
                                    `(= ,s ,variable)
                                    `(<= ,s ,variable ,(1- e))))))))

;;; Named sets

(defun named-range (name)
  (labels ((∪ (&rest rest) (reduce #'csum-union rest))
           (d (a b) (csum-intersection a (csum-complement b)))
           (s (&rest rest) (reduce #'csum-union rest :key (alexandria:compose #'singleton-set #'char-code))))
    (alexandria:eswitch (name :test 'string=)
      ("alpha" (class-set :alpha))
      ("alnum" (∪ (class-set :alpha) (class-set :digit)))
      ("blank" (s #\Space #\Tab))
      ("cntrl" (∪ (range 0 32) (singleton-set 127)))
      ("digit" (class-set :digit))
      ("graph" (csum-complement (∪ (named-range "cntrl") (s #\Space))))
      ("lower" (class-set :lower))
      ("print" (∪ (named-range "graph") (s #\Space)))
      ("punct" (d (named-range "graph") (∪ (class-set :alpha) (class-set :digit))))
      ("space" (∪ (singleton-set 11) (s #\Space #\Return #\Newline #\Tab)))
      ("upper" (class-set :upper))
      ("xdigit" (∪ (class-set "digit") (s #\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f))))))
