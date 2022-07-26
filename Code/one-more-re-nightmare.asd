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
                             (:file "re-types")
                             (:file "nullable")
                             (:file "derivative")
                             (:file "derivative-classes")
                             (:file "empty")
                             (:file "effects")
                             (:file "similar")
                             (:file "tag-sets")
                             (:file "make-dfa")
                             (:file "minimize")))
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
                             (:file "interface")))))
