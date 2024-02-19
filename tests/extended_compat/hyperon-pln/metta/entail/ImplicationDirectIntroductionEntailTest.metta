;; Test Implication Direct Introduction Entail Rule
!(import! &self ImplicationDirectIntroductionEntail.metta)

;; Test formula
!("========== Test formula ==========")
!(idi_formula (Bl True) (Bl False) (STV 1 0))

;; Test axiom
!("========== Test axiom ==========")
!(match &self (⊢ (≞ (→ P Q) $tv)) $tv)

;; Test inductive rule
!("========== Test inductive rule ==========")
!(let* (($pa (≞ (P 42) (Bl True)))
        ($qa (≞ (Q 42) (Bl False)))
        ($pq (≞ (→ P Q) (ETV Empty (STV 1 0))))
        ($an (∉ 42 Empty)))
   (match &self (⊢ $pa $qa $pq $an $conclusion) $conclusion))

;; Backward inference step
;; Disabled cause takes ages
;; !("========== Test backward inference ===========")
;; !(match &self (⊢ $p1 $p2 $p3 $p4 (≞ (→ P Q) $tv))
;;         (Solution $p1 $p2 $p3 $p4 $tv))
