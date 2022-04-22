(asdf:defsystem :one-more-re-nightmare-simd
  :author "Hayley Patton"
  :description "SIMD acceleration for tight loops in DFAs"
  :license "BSD 2-clause"
  :depends-on (:one-more-re-nightmare)
  :serial t
  :components ((:file "package")
               (:file "code-generation")
               #+(and sbcl x86-64)
               #.(if (find-symbol "SIMD-PACK-256-UB8" "SB-VM")
                     '(:file "new-sbcl-x86-64")
                     '(:file "sbcl-x86-64"))
               #-(and sbcl x86-64)
               (:file "i-got-nothing")
               (:file "prefix")
               (:file "prefix-strategy")
               (:file "loop")))
