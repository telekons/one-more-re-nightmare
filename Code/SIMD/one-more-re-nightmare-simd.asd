(asdf:defsystem :one-more-re-nightmare-simd
  :author "Hayley Patton"
  :description "SIMD acceleration for tight loops in DFAs"
  :license "BSD 2-clause"
  :depends-on (:one-more-re-nightmare)
  :serial t
  :components ((:file "package")
               (:file "prefix")
               (:file "code-generation")
               (:file "prefix-strategy")
               (:file "loop")
               (:file "sbcl-x86-64")))
