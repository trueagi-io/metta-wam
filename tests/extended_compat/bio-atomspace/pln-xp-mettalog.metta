!(bind! &kb (new-space))

; inow
(= (include-now $space $file)
  (let* 
	  (($before (atom-count $space))
	   ($_ (load-ascii $space $file))
	   ($after (atom-count $space))
	   ($loaded (- $after $before))
	   ($_ (println! (loaded $file into $space adding $loaded atoms))))
     $after))

!(println! "loading rules")
!(include-now &kb rules.metta)

;!(include-now &kb sample_kb_v2.metta)
;gencode
!(println! "Importing gencode")
!(load-ascii &kb gencode/nodes.metta)
!(include-now &kb gencode/edges.metta)

;uniprot
!(println! "Importing uniprot")
!(include-now &kb uniprot/nodes.metta)
!(include-now &kb uniprot/edges.metta)

;onotology
!(println! "Importing onotology")
!(include-now &kb onotology/nodes.metta)
!(include-now &kb onotology/edges.metta)

;gtex
!(println! "Importing gtex")
!(include-now &kb gtex/eqtl/edges.metta)

;cellxgene co-expression
!(println! "Importing co-expression")
!(include-now &kb cellxgene/edges.metta)

;gaf
!(println! "Importing gaf")
!(include-now &kb gaf/edges.metta)

;tadmap
!(println! "Importing tadmap")
!(include-now &kb tadmap/edges.metta)

;refseq closest gene
!(println! "Importing refseq")
!(include-now &kb refseq/edges.metta)

; !(trace! "size kb" (atom-count &kb))

(: q (-> Atom Atom Atom))
(= (q $space $query)
  (match $space (: $_ $query) $query))
(= (q $space $query)
  (match $space $query $query))

!(println! "gene_name query")
;;!(q &kb (gene_name (gene $X) MT-TT))

!(println! "limit 5 gene_name query")
(: limit! (-> Number Atom Atom))
;;!(limit! 5 (q &kb (gene_name $X $Y)))




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
     (: ($prfabs $prfarg) $ccln))))


;; Fake Curried Backward Chainer
(: fake-bc (-> $a Nat $a))
;; Base case
(= (fake-bc (: $prf $ccln) $_)
;(trace! (⊷ fake-bc-25 (: $prf $ccln) $_)
     (match &kb (: $prf $ccln) (: $prf $ccln)))
;; Recursive step
(= (fake-bc (: ($prfabs $prfarg) $ccln) (S $k))
; (trace! (⊷ fake-bc-29 (: ($prfabs $prfarg) $ccln) (S $k))
   (quote 
	 (let* (((: $prfabs (-> $prms $ccln)) (fake-bc (: $prfabs (-> $prms $ccln)) $k))
          ((: $prfarg $prms) (fake-bc (: $prfarg $prms) $k)))
     (: ($prfabs $prfarg) $ccln))))

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

;;!(pragma! eval trace)
!(println! "bc relevant-go")
;Proof for the relation between (sequence_variant rs9930506) and regulation of cold-induced thermogenesis (GO:0120161)
!(bc (: $prf (relevant-go (ontology_term GO:0120161) (sequence_variant rs9930506))) (fromNumber 9))
!(println! "finished bc relevant-go")
;; return (let* (((: $prfabs#826 (-> $prms#830 (relevant-go (ontology_term GO:0120161) (sequence_variant rs9930506)))) (bc (: $prfabs#826 (-> $prms#830 (relevant-go (ontology_term GO:0120161) (sequence_variant rs9930506)))) (S (S (S (S (S (S (S (S Z)))))))))) ((: $prfarg#827 $prms#830) (bc (: $prfarg#827 $prms#830) (S (S (S (S (S (S (S (S Z))))))))))) (: ($prfabs#826 $prfarg#827) (relevant-go (ontology_term GO:0120161) (sequence_variant rs9930506))))


;!(println! "fake-bc relevant-go")
;!(fake-bc (: $prf (relevant-go (ontology_term GO:0120161) (sequence_variant rs9930506))) (fromNumber 9))
;!(println! "finished fake-bc relevant-go")
;; (quote (let* (((: $prfabs#835 (-> $prms#839 (relevant-go (ontology_term GO:0120161) (sequence_variant rs9930506)))) (bc (: $prfabs#835 (-> $prms#839 (relevant-go (ontology_term GO:0120161) (sequence_variant rs9930506)))) (S (S (S (S (S (S (S (S Z)))))))))) ((: $prfarg#836 $prms#839) (bc (: $prfarg#836 $prms#839) (S (S (S (S (S (S (S (S Z))))))))))) (: ($prfabs#835 $prfarg#836) (relevant-go (ontology_term GO:0120161) (sequence_variant rs9930506)))) )


