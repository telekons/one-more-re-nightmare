(in-package :one-more-re-nightmare)

(defvar *code-cache*
  (make-hash-table :test 'equal))
(defvar *code-lock*
  (bt:make-lock "Compiled code cache lock"))

(defvar *string-types*
  (alexandria:map-product #'list
                          '(simple-array array)
                          '(character base-char)
                          '(1)))

(defun find-code (regular-expression type-specifier)
  (bt:with-lock-held (*code-lock*)
    (multiple-value-bind (code present?)
        (gethash (list regular-expression type-specifier)
                 *code-cache*)
      (when present?
        (return-from find-code code))))
  (multiple-value-bind (function groups)
      (compile-regular-expression
       regular-expression
       :layout (make-layout :array-type type-specifier))
    (bt:with-lock-held (*code-lock*)
      (setf (gethash (list (copy-seq regular-expression)
                           type-specifier)
                     *code-cache*)
            (list function groups)))))

(defun string-type-of (string)
  (loop for type in *string-types*
        when (typep string type)
          do (return-from string-type-of type))
  (error 'type-error
         :datum string
         :expected-type 'string))
