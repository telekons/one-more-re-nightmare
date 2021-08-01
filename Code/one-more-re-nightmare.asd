(asdf:defsystem :one-more-re-nightmare
  :depends-on (:trivia :alexandria :babel :trivial-garbage :esrap :trivial-indent)
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
               #+(or)
               (:module "Boyer-Moore-Horspool"
                :components ((:file "prefix")
                             (:file "boyer-moore-horspool")))
               (:module "Interface"
                :components ((:file "syntax")
                             (:file "convert-to-bytes")
                             (:file "interface")))))
