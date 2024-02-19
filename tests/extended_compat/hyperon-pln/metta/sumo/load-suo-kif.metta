;; MeTTa code to convert SUO-KIF format to MeTTa.

;; Takes a SUO-KIF term and produces an equivalent MeTTa term.  For
;; now, all it does is replace SUO-KIF variables, such as ?X, by MeTTa
;; variables, such as $X.
(= (suo-kif-to-metta $x)
   ;; Trick to distinguish symbol versus expression
   (case (car-atom $x)
     (;; Symbol
      ((Error $a $msg) NEXT)
      ;; Expression
      ($_ NEXT)))
)
