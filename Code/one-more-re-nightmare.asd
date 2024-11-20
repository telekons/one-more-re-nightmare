(asdf:defsystem :one-more-re-nightmare
  :author "Hayley Patton"
  :description "A regular expression compiler"
  :license "BSD 2-clause"
  :depends-on (:trivia :alexandria :babel
               :esrap :trivial-indent
               :dynamic-mixins :stealth-mixin
               :bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:module "DFA-construction"
                :components ((:file "type")
                             (:file "sets")
                             (:file "hash-cons")
                             (:file "re-types")
                             (:file "nullable")
                             (:file "tag-sets")
                             (:file "derivative")
                             (:file "derivative-classes")
                             (:file "empty")
                             (:file "effects")
                             (:file "similar")
                             (:file "make-dfa")))
               (:module "Compiler"
                :components ((:file "layout")
                             (:file "compilation-strategy")
                             (:file "length-inference")
                             (:file "optimize-settings")
                             (:file "code-generation")))
               (:module "Interface"
                :components ((:file "syntax")
                             (:file "convert-to-bytes")
                             (:file "code-cache")
                             (:file "lint")
                             (:file "interface"))))
  ;; Trivia emits warnings at compile-time that type-i can't infer
  ;; types for some patterns.
  :around-compile (lambda (thunk)
                    (handler-bind ((warning
                                     (lambda (c)
                                       (when (typep c (find-symbol "FAILED-TYPE-INFERENCE" "TYPE-I"))
                                         (muffle-warning)))))
                      (funcall thunk))))
