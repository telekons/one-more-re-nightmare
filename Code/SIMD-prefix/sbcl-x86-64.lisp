(in-package :one-more-re-nightmare)

(defmacro define-boring-vop (name args result &body generator)
  `(progn
     (sb-vm::define-vop (,name)
       (:translate ,name)
       (:policy :fast-safe)
       (:args ,@(loop for (name nil . rest) in args
                      collect (cons name rest)))
       (:arg-types ,@(mapcar #'second args))
       (:results (,(first result) ,@(rest (rest result))))
       (:result-types ,(second result))
       (:generator 0 ,@generator))))

(defmacro define-op (name args instruction-name)
  `(progn
     (sb-c:defknown ,name
         ,(loop for nil in args collect '(sb-ext:simd-pack-256 integer))
         (sb-ext:simd-pack-256 integer)
         (sb-c:foldable sb-c:flushable sb-c:movable)
       :overwrite-fndb-silently t)
     (define-boring-vop ,name
         ,(loop for arg in args
                collect `(,arg sb-vm::simd-pack-256-int :scs (sb-vm::int-avx2-reg)))
         (result sb-vm::simd-pack-256-int :scs (sb-vm::int-avx2-reg))
       (sb-vm::inst ,instruction-name result ,@args))))

(defconstant one-more-re-nightmare.vector-primops:+v-length+ 8)

(in-package :sb-vm)

(one-more-re-nightmare::define-op
    one-more-re-nightmare.vector-primops:v-and (a b) vpand)

(one-more-re-nightmare::define-op
    one-more-re-nightmare.vector-primops:v-or (a b) vpor)

(defknown one-more-re-nightmare.vector-primops:v-not
    ((simd-pack-256 integer))
    (simd-pack-256 integer)
    (foldable flushable movable)
  :overwrite-fndb-silently t)

(define-vop (one-more-re-nightmare.vector-primops:v-not)
  (:translate one-more-re-nightmare.vector-primops:v-not)
  (:policy :fast-safe)
  (:args (value :scs (int-avx2-reg)))
  (:arg-types simd-pack-256-int)
  (:results (result :scs (int-avx2-reg)))
  (:result-types simd-pack-256-int)
  (:temporary (:sc int-avx2-reg) ones)
  (:generator 0
    (inst vpcmpeqd ones ones ones)      ; get all 1s
    (inst vpxor result ones value)))    ; 1111... (+) A = ¬A

(defknown one-more-re-nightmare.vector-primops:v-load
    ((simple-array character 1) sb-int:index)
    (simd-pack-256 integer)
    (foldable flushable movable)
  :overwrite-fndb-silently t)

;; This is a signed comparison, but as there are fewer than 2³¹
;; Unicode characters, no one needs to know that.
(one-more-re-nightmare::define-op
    one-more-re-nightmare.vector-primops:v32> (a b) vpcmpgtd)

(one-more-re-nightmare::define-op
    one-more-re-nightmare.vector-primops:v32= (a b) vpcmpeqd)

(one-more-re-nightmare::define-boring-vop
    one-more-re-nightmare.vector-primops:v-load
    ((string simple-character-string :scs (descriptor-reg))
     (index tagged-num :scs (any-reg)))
    (result simd-pack-256-int :scs (int-avx2-reg))
  (inst vmovdqu result
        (ea (- (* vector-data-offset n-word-bytes)
               other-pointer-lowtag)
            ;; Characters are 4 bytes, fixnums have a trailing 0 so
            ;; just multiply by 2.
            string index 2)))

(defknown one-more-re-nightmare.vector-primops:find-first-set
    ((unsigned-byte 64))
    (unsigned-byte 6)
    (foldable flushable movable)
  :overwrite-fndb-silently t)

(one-more-re-nightmare::define-boring-vop
    one-more-re-nightmare.vector-primops:find-first-set
    ((integer unsigned-num :scs (unsigned-reg)))
    (result unsigned-num :scs (unsigned-reg))
  (inst bsf result integer))
