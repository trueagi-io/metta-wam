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
!(include-now &kb sample_kb_v2.metta)
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
;; Recursive step
(= (bc-i (: $prf $ccln) $_)
	 (let () () ;(println! (bc-base (: $prf $ccln)))
		  (match &kb (: $prf $ccln) 
			   (let () () ; (println! (bc-base-ground (: $prf $ccln))) 
				 (: $prf $ccln)))))
(= (bc-i (: ($prfabs $prfarg) $ccln) (S $k))
  (let () () ; (println! (bc-rec (: ($prfabs $prfarg) $ccln) (S $k))) 
	 (let* (((: $prfabs (-> $prms $ccln)) (bc-i (: $prfabs (-> $prms $ccln)) $k))
			   ((: $prfarg $prms) (bc-i (: $prfarg $prms) $k)))
			   (: ($prfabs $prfarg) $ccln))))


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

!(test1)

!(bc-i (: $prf (in_tad_with (sequence_variant rs15) (gene d))) (S (S (S (S Z)))))

;.{?- xlisting(bc-i)}.

;!(bc-fn (: $prf (in_tad_with (sequence_variant rs1421085) (gene ENSG00000177508))) (fromNumber 4))

;Prove that IRX3 is relevant-gene for (sequence_variant rs1421085) using R3 and R2
;!(println! "Prove that IRX3 is relevant gene for (sequence_variant rs1421085) using R2 & R3")
;!(bc (: $prf (relevant_gene (gene ENSG00000177508) (sequence_variant rs1421085))) (fromNumber 5))

;;Prove that FABP4 is relevant-gene for (sequence_variant rs1421085) using R5
;!(bc (: $prf (relevant_gene (gene ENSG00000170323) (sequence_variant rs1421085))) (fromNumber 7))

;Proof for the relation between (sequence_variant rs1421085) and regulation of fat cell differentiation (GO:0045598)
; !(bc (: $prf (relevant_go (ontology_term GO:0045598) (sequence_variant rs1421085))) (fromNumber 9))
; [(Error (import! GroundingSpace-0x559a73053968 gencode/nodes.metta) Illegal module name: gencode/nodes.metta)]


