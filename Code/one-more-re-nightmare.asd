(asdf:defsystem :one-more-re-nightmare
  :depends-on (:trivia :alexandria :babel
               :esrap :trivial-indent
               :dynamic-mixins :stealth-mixin)
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
                             (:file "make-dfa")))
               (:module "Compiler"
                :components ((:file "compilation-strategy")
                             (:file "length-inference")
                             (:file "optimize-settings")
                             (:file "aliases")
                             (:file "code-generation")))
               #+(or)
               (:module "Boyer-Moore-Horspool"
                :components ((:file "prefix")
                             (:file "boyer-moore-horspool")))
               (:module "Interface"
                :components ((:file "syntax")
                             (:file "convert-to-bytes")
                             (:file "interface")))))
