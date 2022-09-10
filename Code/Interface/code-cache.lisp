(in-package :one-more-re-nightmare)

(defvar *code-cache*
  (make-hash-table :test 'equal))
(defvar *code-lock*
  (bt:make-lock "Compiled code cache lock"))

(defstruct (compiled-regular-expression (:conc-name cre-))
  (codes (alexandria:required-argument) :type simple-vector)
  original-re)
(deftype re-designator ()
  `(or compiled-regular-expression string))

(defvar *string-types*
  (alexandria:map-product #'list
                          '(simple-array array)
                          '(character base-char)
                          '(1)))

(defun compile-regular-expression (expression)
  (make-compiled-regular-expression
   :codes (coerce
           (loop for type in *string-types*
                 collect (find-code expression type))
           'vector)
   :original-re expression))

(defmethod print-object ((cre compiled-regular-expression) stream)
  (print-unreadable-object (cre stream :type t)
    (write-string (cre-original-re cre) stream)))

(defvar *type-dispatcher*
  (compile nil
           `(lambda (cre type)
              (cond
                ,@(loop for type in *string-types*
                        for index from 0
                        collect `((eq type ',type) (svref (cre-codes cre) ,index)))))))

(defun find-code (regular-expression type-specifier)
  (when (compiled-regular-expression-p regular-expression)
    (return-from find-code
      (funcall *type-dispatcher* regular-expression type-specifier)))
  (bt:with-lock-held (*code-lock*)
    (multiple-value-bind (code present?)
        (gethash (list regular-expression type-specifier)
                 *code-cache*)
      (when present?
        (return-from find-code code))))
  (multiple-value-bind (function groups)
      (%compile-regular-expression
       regular-expression
       :layout (make-layout :array-type type-specifier))
    (bt:with-lock-held (*code-lock*)
      (setf (gethash (list (copy-seq regular-expression)
                           type-specifier)
                     *code-cache*)
            (cons function groups)))))

(declaim (ftype (function (string) t) string-type-of))
(defun string-type-of (string)
  (loop for type in *string-types*
        when (typep string type)
          do (return-from string-type-of type))
  (error 'type-error
         :datum string
         :expected-type 'string))
