(asdf:defsystem :one-more-re-nightmare-simd
  :depends-on (:one-more-re-nightmare)
  :serial t
  :components ((:file "package")
               (:file "prefix")
               (:file "code-generation")
               (:file "strategy")
               (:file "sbcl-x86-64")))
