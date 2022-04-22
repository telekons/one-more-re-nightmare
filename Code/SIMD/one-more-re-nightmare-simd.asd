(asdf:defsystem :one-more-re-nightmare-simd
  :author "Hayley Patton"
  :description "SIMD acceleration for tight loops in DFAs"
  :license "BSD 2-clause"
  :depends-on (:one-more-re-nightmare)
  :serial t
  :components ((:file "package")
               (:file "code-generation")
               (:file "new-sbcl-x86-64")
               (:file "prefix")
               (:file "prefix-strategy")
               (:file "loop")))
