(asdf:defsystem :one-more-re-nightmare
  :depends-on (:trivia :alexandria :trivial-garbage)
  :serial t
  :components ((:file "package")
               (:file "type")
               (:file "sets")
               (:file "re-types")
               (:file "nullable")
               (:file "derivative")
               (:file "derivative-classes")
               (:file "interpret-regexp")
               (:file "compile-regexp")
               (:file "interface")))
