(in-package :sb-vm)

;;;; This VOP lies to SBCL about types. If we return a (UNSIGNED-BYTE
;;;; 64), then it will not bother to box the value for no reason.

(sb-c:defknown one-more-re-nightmare::%string-ref ((simple-array character 1) sb-int:index)
    (unsigned-byte 64)
    (foldable movable flushable)
  :overwrite-fndb-silently t)

(define-vop (one-more-re-nightmare::%string-ref)
  (:translate one-more-re-nightmare::%string-ref)
  (:policy :fast-safe)
  (:args (string :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types simple-character-string tagged-num)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-byte-64)
  (:generator 1
   (inst mov :dword value
         (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
             string index 2))))
