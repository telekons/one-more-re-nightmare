(defpackage :one-more-re-nightmare.vector-primops
  (:use)
  (:export #:v-and #:v-or #:v-not #:all-of
           #:v-and8 #:v-and32 #:v-or8 #:v-or32 #:v-not8 #:v-not32
           #:v32> #:v32= #:v8> #:v8= #:v8-
           #:v-broadcast32 #:v-movemask32 #:v-movemask8 #:v-broadcast8
           #:v-load32 #:v-load8 #:+v-length+ #:find-first-set))
