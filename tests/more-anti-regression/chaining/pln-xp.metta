
; !(bind! &kb (new-space))
; !(bind! &kb2 (new-space))
!(import! &kb2 rules)
; !(bind! &kb3 (new-space))
!(load-ascii &kb3 sample_kb_v2.metta)
;gencode


!(match (superpose (&kb2 &kb3)) $x (add-atom &kb $x))

;; Define Nat
(: Nat Type)
(: Z Nat)
(: S (-> Nat Nat))

;; Define <=
(: <= (-> $a $a Bool))
(= (<= $x $y) (or (< $x $y) (== $x $y)))

;; Define cast functions between Nat and Number
(: fromNumber (-> Number Nat))
(= (fromNumber $n) (if (<= $n 0) Z (S (fromNumber (- $n 1)))))
(: fromNat (-> Nat Number))
(= (fromNat Z) 0)
(= (fromNat (S $k)) (+ 1 (fromNat $k)))

;; Curried Backward Chainer
(: bc (-> $a Nat $a))
;; Base case
(= (bc (: $prf $ccln) $_)
;(trace! (⊷ bc-25 (: $prf $ccln) $_)
     (match &kb (: $prf $ccln) (: $prf $ccln)))
;; Recursive step
(= (bc (: ($prfabs $prfarg) $ccln) (S $k))
; (trace! (⊷ bc-29 (: ($prfabs $prfarg) $ccln) (S $k))
   (let* (((: $prfabs (-> $prms $ccln)) (bc (: $prfabs (-> $prms $ccln)) $k))
          ((: $prfarg $prms) (bc (: $prfarg $prms) $k)))
     (: ($prfabs $prfarg) $ccln)))

;;;;;;;;;;;;;;;;;;;;;
;; Forward chainer ;;
;;;;;;;;;;;;;;;;;;;;;

;; Curried Forward Chainer
(: fc (-> $a Nat $a))
;; Base case.  Beware that the provided source is assumed to be true.
(= (fc (: $proof $premise) $_) 
     ; (trace! (⊷ fc-39 (: $proof $premise) $_)
          (: $proof $premise))
;; Recursive step
(= (fc (: $prfabs (-> $prms $ccln)) (S $k)) 
;     (trace! (⊷ fc-43 (: $prfabs (-> $prms $ccln)) (S $k))
     (let (: $prfarg $prms) (bc (: $prfarg $prms) $k)
          ; (trace! (⊏ fc-43 (: ($prfabs $prfarg) $ccln) $k)
               (fc (: ($prfabs $prfarg) $ccln) $k)))

(= (fc (: $prfarg $premise) (S $k))
;   (trace! (⊷ fc-49 (: $prfarg $premise) (S $k))
     (let (: $prfabs (-> $premise $ccln)) (bc (: $prfabs (-> $premise $ccln)) $k)
          ; (trace! (⊏ fc-49 (: ($prfabs $prfarg) $ccln) $k) 
               (fc (: ($prfabs $prfarg) $ccln) $k)))


(= (communicative-coexpressed (communicative-coexpressed $x)) $x)
(= (communicative-interaction (communicative-interaction $x)) $x)

!(bc (: $prf (in_tad_with (sequence_variant rs15) (gene d))) (fromNumber 4))

;Prove that IRX3 is in-tad-with (sequence_variant rs9930506) using R2
;!(println! "Prove that IRX3 is in-tad-with (sequence_variant rs9930506) using R2")
;!(bc (: $prf (in_tad_with (sequence_variant rs9930506) (gene ENSG00000177508))) (fromNumber 4))

;Prove that IRX3 is relevant-gene for (sequence_variant rs9930506) using R3 and R2
; !(println! "Prove that IRX3 is relevant gene for (sequence_variant rs9930506) using R2 & R3")
; !(bc (: $prf (relevant_gene (gene ENSG00000177508) (sequence_variant rs9930506))) (fromNumber 5))

;Proof for the relation between (sequence_variant rs9930506) and regulation of cold-induced thermogenesis (GO:0120161)
;!(println! "Proof for the relation between (sequence_variant rs9930506) and regulation of cold-induced thermogenesis (GO:0120161)")
;!(bc (: $prf (relevant_go (ontology_term GO:0120161) (sequence_variant rs9930506))) (fromNumber 9))
