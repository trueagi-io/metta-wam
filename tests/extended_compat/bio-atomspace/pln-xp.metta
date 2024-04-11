;!(bind! &kb (new-space))
; !(bind! &kb2 (new-space))
; inow
(= (include-now $space $file)
  (let* 
	  (($before (atom-count $space))
	   ($_ (load-ascii $space $file))
	   ($after (atom-count $space))
	   ($loaded (- $after $before))
	   ($_ (println! (loaded $file into $space adding $loaded atoms before-after $before $after))))
	 ()))

!(import! &kb rules)

; !(bind! &kb3 (new-space))
; !(include-now &kb sample_kb_v2.metta)
;gencode
!(println! "Importing gencode")
!(include-now &kb /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/metta-wam/tests/extended_compat/bio-atomspace/gencode/nodes.metta)
!(include-now &kb /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/metta-wam/tests/extended_compat/bio-atomspace/gencode/edges.metta)
;uniprot
!(println! "Importing uniprot")
!(include-now &kb /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/metta-wam/tests/extended_compat/bio-atomspace/uniprot/nodes.metta)
!(include-now &kb /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/metta-wam/tests/extended_compat/bio-atomspace/uniprot/edges.metta)
;onotology
!(println! "Importing onotology")
!(include-now &kb /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/metta-wam/tests/extended_compat/bio-atomspace/onotology/nodes.metta)
!(include-now &kb /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/metta-wam/tests/extended_compat/bio-atomspace/onotology/edges.metta)
;gtex
!(println! "Importing gtex")
!(include-now &kb /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/metta-wam/tests/extended_compat/bio-atomspace/gtex/eqtl/edges.metta)
;cellxgene co-expression using brown preadipocyte expression
!(println! "Importing co-expression")
!(include-now &kb /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/metta-wam/tests/extended_compat/bio-atomspace/cellxgene/edges.metta)
;gaf
!(println! "Importing gaf")
!(include-now &kb /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/metta-wam/tests/extended_compat/bio-atomspace/gaf/edges.metta)
;tadmap
!(println! "Importing tadmap")
!(include-now &kb /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/metta-wam/tests/extended_compat/bio-atomspace/tadmap/edges.metta)
;refseq closest gene
!(println! "Importing refseq")
!(include-now &kb /opt/logicmoo_workspace/packs_sys/logicmoo_opencog/MeTTa/metta-wam/tests/extended_compat/bio-atomspace/refseq/edges.metta)
!(println! "Importing Done...running inference")


; !(match (superpose (&kb2 &kb3)) $x (add-atom &kb $x))

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

(= (less_than $x $y) (if (< $x $y) (: (less_than $x $y) (≪ $x $y)) (empty)))

;; Curried Backward Chainer
(: bc-i (-> $a Nat $a))
;; Base case
(= (bc-i (: $prf $ccln) $_)
     (let () (println! (bc-base (: $prf $ccln)))
          (match &kb (: $prf $ccln) 
               (let () (println! (bc-base-ground (: $prf $ccln))) (: $prf $ccln)))))

;; Recursive step
(= (bc-i (: ($prfabs $prfarg) $ccln) (S $k))
  (let () (println! (bc-rec (: ($prfabs $prfarg) $ccln) (S $k))) 
     (let* (((: $prfabs (-> $prms $ccln)) (bc-i (: $prfabs (-> $prms $ccln)) $k))
               ((: $prfarg $prms) (bc-i (: $prfarg $prms) $k)))
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

; !(bc (: $prf (in_tad_with (sequence_variant rs15) (gene d))) (fromNumber 4))

;Prove that IRX3 is in-tad-with (sequence_variant rs1421085) using R2
(= (test1)
( let*
  (($_ (println! "Prove that IRX3 is in-tad-with (sequence_variant rs1421085) using R2"))
   ($res (println! (bcc-fn (: $prf (in_tad_with (sequence_variant rs1421085) (gene ENSG00000177508))) (fromNumber 4))))
   ($_ (println! (expected (: ((tad-eqtl (((snp-gene-tad in_tad_region-chr16_53550000_55450000_GRCh38-ENSG00000140718) in_tad_region-chr16_5355000    0_55450000_GRCh38-ENSG00000177508) closest_gene-rs1421085-ENSG00000140718)) eqtl-rs1421085-ENSG00000177508-Pancreas) (relevant_gene (gene ENSG00000177508) (sequence_variant rs1421085))))))
	) $res))

;!(test1)

;!(bc-fn (: $prf (in_tad_with (sequence_variant rs1421085) (gene ENSG00000177508))) (fromNumber 4))

;Prove that IRX3 is relevant-gene for (sequence_variant rs1421085) using R3 and R2
;!(println! "Prove that IRX3 is relevant gene for (sequence_variant rs1421085) using R2 & R3")
;!(bc (: $prf (relevant_gene (gene ENSG00000177508) (sequence_variant rs1421085))) (fromNumber 5))

;;Prove that FABP4 is relevant-gene for (sequence_variant rs1421085) using R5
;!(bc (: $prf (relevant_gene (gene ENSG00000170323) (sequence_variant rs1421085))) (fromNumber 7))

;Proof for the relation between (sequence_variant rs1421085) and regulation of fat cell differentiation (GO:0045598)
; !(bc (: $prf (relevant_go (ontology_term GO:0045598) (sequence_variant rs1421085))) (fromNumber 9))
; [(Error (import! GroundingSpace-0x559a73053968 gencode/nodes.metta) Illegal module name: gencode/nodes.metta)]

