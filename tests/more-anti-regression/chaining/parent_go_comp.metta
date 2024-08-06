
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

!(pragma! compile full)

(: bc (-> $a Nat hyperon::space::DynSpace $a))
;; Base case
(= (bc (: $prf $ccln) $_1 $space) (match $space (: $prf $ccln) (: $prf $ccln)))
;; recursive case
(= (bc (: ($prfabs $prfarg) $ccln) (S $k) $space)
   (let* (((: $prfabs (-> $prms $ccln)) (bc (: $prfabs (-> $prms $ccln)) $k $space))
		  ((: $prfarg $prms) (bc (: $prfarg $prms) $k $space)))
	 (: ($prfabs $prfarg) $ccln)))


!(import! &kb go_rel)

; Abdulrahman's member/3 test
!(assertEqualToResult
  (bc
	(: $prf
	  (member $g $o $k))
	(fromNumber 3) &kb)
  ( (:
	  ( (go-level go_parent-d-c) ((go-level go_parent-c-b) (go-level go_gene-b-g)))
	  (member (gene g) (ontology_term d) (S (S Z))))
	(:
	  ( (go-level go_parent-c-b) (go-level go_gene-b-g))
	  (member (gene g)  (ontology_term c) (S Z)))
	(:
	  (go-level go_gene-b-g)
	  (member (gene g) (ontology_term b) Z))
	(:
	  (go-level go_gene-a-g)
	  (member (gene g) (ontology_term a) Z))))
