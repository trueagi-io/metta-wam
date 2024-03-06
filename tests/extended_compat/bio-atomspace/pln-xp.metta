!(bind! &kb (new-space))
!(bind! &kb2 (new-space))
!(bind! &kb3 (new-space))
;!(import! &kb2 rules.metta)
;!(import! &kb3 sample_kb_v2.metta)
;gencode
!(println! "Importing gencode")
!(import! &kb3 gencode/nodes.metta)
!(import! &kb3 gencode/edges.metta)
;uniprot
!(println! "Importing uniprot")
!(import! &kb3 uniprot/nodes.metta)
!(import! &kb3 uniprot/edges.metta)
;onotology
!(println! "Importing onotology")
!(import! &kb3 onotology/nodes.metta)
!(import! &kb3 onotology/edges.metta)
;gtex
!(println! "Importing gtex")
!(import! &kb3 gtex/eqtl/edges.metta)
;cellxgene co-expression
!(println! "Importing co-expression")
;;!(import! &kb3 cellxgene/edges.metta)
;gaf
!(println! "Importing gaf")
!(import! &kb3 gaf/edges.metta)
;tadmap
!(println! "Importing tadmap")
;;!(import! &kb3 tadmap/edges.metta)
;refseq closest gene
!(println! "Importing refseq")
;;!(import! &kb3 refseq/edges.metta)

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

;Proof for the relation between (sequence_variant rs9930506) and regulation of cold-induced thermogenesis (GO:0120161)
!(bc (: $prf (relevant-go (ontology_term GO:0120161) (sequence_variant rs9930506))) (fromNumber 9))