(asdf:defsystem :one-more-re-nightmare-tests
  :depends-on (:parachute :one-more-re-nightmare :bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "tests")
               (:file "regrind")))
