#lang scribble/base

@(require scribble/core
          scribble/html-properties
          scribble/latex-properties
          racket/list)
@(provide definition-section definitions
          defthing defun defmacro defgeneric defmethod
          defaccessor defreader definitarg
          defclass defprotoclass defvar
          define-condition
          cl lisp-code
          param &keyword &key &optional &rest &allow-other-keys &body
          term concept
          bnf rule tag-rule
          todo note
          image/width
          code-template var
          landscape centered-block)

@(define spec-macro-additions
         (list (make-tex-addition "spec-macros.tex")))
@(define block-thing-additions
         (cons (alt-tag "div") spec-macro-additions))

@(define float-right-style
   (make-style "FloatRight" spec-macro-additions))
@(define sc-style
   (make-style "SmallCaps" spec-macro-additions))
@(define definitions-style
   (make-style "Definitions" block-thing-additions))
@(define indent-style
   (make-style "Indent" spec-macro-additions))
@(define landscape-style
   (make-style "Landscape" block-thing-additions))
@(define centered-block-style
   (make-style "CenteredBlock" block-thing-additions))
@(define centered-container-style
   (make-style "CenteredContainer" block-thing-additions))
@(define param-style
   (make-style "Param" spec-macro-additions))
@(define defun-name-style
   (make-style "DefunName" (cons (alt-tag "b") spec-macro-additions)))
@(define lambda-list-style
   (make-style "LambdaList" (cons (alt-tag "i") spec-macro-additions)))

@(define @float-right[thing]
   (elem thing #:style float-right-style))
@(define @indent[thing]
   (nested thing #:style 'inset))
@(define @sc[thing]
   (elem thing #:style sc-style))
@(define (landscape . stuff)
   (nested stuff #:style landscape-style))
@(define (centered-block . stuff)
   (nested (nested stuff #:style centered-block-style)
           #:style centered-container-style))

@(define @defthing[name more type #:index? [index? #t]]
   (let ([base (list (elem name #:style defun-name-style)
                     " " (elem (list more (float-right (italic type)))
                               #:style lambda-list-style))])
    (if index?
        (index* (list name) (list (list (string-append name " " type)))
                base)
        base)))

@(define (definition-section name . things)
   (list (linebreak)
         (sc name)
         (linebreak)
         things))

@(define (definitions . stuff)
   (nested #:style definitions-style
           stuff))

@(define (def-function-thing name arguments type #:index? [index? #t])
   (list (defthing name (list " " (italic arguments)) type #:index? index?)
         (linebreak)))

@(define (defun name . arguments)
   (def-function-thing name arguments "Function"))
@(define (defmacro name . arguments)
   (def-function-thing name arguments "Macro"))
@(define (defgeneric name . arguments)
   (def-function-thing name arguments "Generic Function"))
@(define (defmethod name #:qualifier [q ""] . arguments)
   (def-function-thing name arguments
                       (if (string=? q "")
                           "Method"
                           (string-append q " Method"))
                       #:index? #f))
@(define (defaccessor name . arguments)
   (def-function-thing name arguments "Accessor"))
@(define (defreader name . arguments)
   (def-function-thing name arguments "Reader"))
@(define (definitarg name)
   (defthing name "" "Initarg"))
   
@(define (interleave v l)
   (reverse
    (for/fold ([a '()])
              ([x (in-list l)])
       (list* x v a))))

@(define (superclass-part superclasses)
   (if (or (equal? superclasses "")
           (equal? superclasses '())
           (equal? superclasses '("")))
       ""
       (interleave " " (cons "<:" superclasses))))

@(define (defclass name . superclasses)
   (defthing name (superclass-part superclasses) "Class"))

@(define (define-condition name . supertypes)
   (def-function-thing name (superclass-part supertypes) "Condition Type"))

@(define (defprotoclass name)
   (defthing name "" "Protocol Class"))

@(define (defvar name initial-value)
   (defthing name (list "initially " initial-value) "Variable"))

@(define (&keyword name)
   (elem #:style 'tt "&" name))

@(define (param . stuff) (elem stuff #:style param-style))
@(define &key @&keyword{key})
@(define &rest @&keyword{rest})
@(define &body @&keyword{body})
@(define &optional @&keyword{optional})
@(define &allow-other-keys @&keyword{allow-other-keys})
@(define cl tt)
@(define (lisp-code . stuff)
   (nested #:style 'code-inset
    (apply verbatim stuff)))

@(define term italic)
@(define (concept name)
   (index name (italic name)))

@(define (groups x n)
   (if (> (length x) n)
       (cons (take x n) (groups (drop x n) n))
       (list x)))

@(define (bnf . rules)
   (tabular #:sep (hspace 1)
            #:column-properties '(right center left right)
            #:style (style #f (list (attributes '((style . "width: 100%")))))
            (apply append rules)))
@(define (rule name . generates)
   (for/list ([rhs (in-list (groups generates 2))]
              [n   (in-naturals)])
      (if (zero? n)
          (list* name "::=" rhs)
          (list* ""   "|"   rhs))))
@(define (tag-rule lhs tag rhs)
   (rule lhs (list (tt tag) " " rhs)))

@(define (todo . stuff)
   (list (bold "TODO: ") stuff))
@(define (note . stuff)
   (list (bold "NOTE: ") stuff))

@(define (image/width width pathname #:scale [scale 0.5])
   (image #:style (style #f
                        (list
                         (attributes
                          `((style . ,(string-append "height: auto; width: " width))))
                         (command-optional (list (string-append "width=" width)))))
          #:scale scale
          pathname))

@(define-syntax code-template
   (syntax-rules [var]
     [(code-template) '()]
     [(code-template (var x) rest ...)
      (cons x (code-template rest ...))]
     [(code-template x rest ...)
      (cons (tt x) (code-template rest ...))]))
@(define (var _) (error "No var outside code-template"))
