(asdf:defsystem :one-more-re-nightmare-tests
  :author "Hayley Patton"
  :description "Tests for a regular expression compiler"
  :license "BSD 2-clause"
  :depends-on (:parachute :one-more-re-nightmare :lparallel)
  :serial t
  :components ((:file "package")
               (:file "tests")
               (:file "regrind")))
