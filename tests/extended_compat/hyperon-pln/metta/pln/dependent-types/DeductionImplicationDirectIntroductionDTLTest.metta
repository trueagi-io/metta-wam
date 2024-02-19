;; Test DeductionDTL and ImplicationDirectionIntroductionDTL rules

;; Import the rules
!(import! &self ImplicationDirectIntroductionDTL.metta)
!(import! &self DeductionDTL.metta)
!(import! &self ../../synthesis/Synthesize.metta)

;; Knowledge base
(: kb (-> Atom))
(= (kb) (superpose ((: Pm (≞ P (STV 0.2 0.3)))
                    (: Qm (≞ Q (STV 0.3 0.2)))
                    (: Rm (≞ R (STV 0.4 0.1)))
                    (: P2 (⊷ P (fromNumber 2) True))
                    (: Q2 (⊷ Q (fromNumber 2) False))
                    (: P7 (⊷ P (fromNumber 7) True))
                    (: Q7 (⊷ Q (fromNumber 7) True))
                    (: QRm (≞ (→ Q R) (STV 0.9 0.7))))))
;; All axioms are placed in the kb as well
(= (kb) (zero-lt-succ-axiom))

;; Rule base
(: rb (-> Atom))
(= (rb) (superpose ((succ-monotonicity-rule)
                    (deduction-rule)
                    (implication-direct-introduction-base-rule)
                    (implication-direct-introduction-recursive-rule)
                    (evidence-elimination-rule))))

;; Test a combination of deduction and implication direct introduction
;;
;; Build the following inference tree
;;
;;                                                                                      -----(ZeroLTSucc)
;;                                                                                      0 ⍃ 5
;;                                                 --------(P2)     --------(Q2)        -----(SuccMonotonicity)
;;                                                 (P 2) = ⊤        (Q 2) = ⊥           1 ⍃ 6
;;                     --------(P7)  --------(Q7)  --------------------------(IDIBase)  -----(SuccMonotonicity)
;;                     (P 7) = ⊤     (Q 7) = ⊤     P → Q ≞ ((:: 2 ∅), <0 0.5>)          2 ⍃ 7
;;                     ---------------------------------------------------------------------(IDIRecursive)
;;                                                 P → Q ≞ (ETV (:: 7 (:: 2 ∅)) $pqtv)
;;                                                             |
;; ------------(Pm)  ------------(Qm)  ------------(Rm)  -------------(EvidenceElimination)  ----------------(QRm)
;; P ≞ <0.2 0.3>     Q ≞ <0.3 0.2>     R ≞ <0.4 0.1>     P → Q ≞ $pqtv                       Q → R ≞ <0.9 0.7>
;; ----------------------------------------------------------------------------------------------------------(Deduction)
;;                                                       P → R ≞ $prtv
(: deduction_idi_prf (≞ (→ P Q) $tv))
(= deduction_idi_prf (Deduction Pm Qm Rm (IDIBase P2 Q2) QRm))
! deduction_idi_prf
!(get-type deduction_idi_prf)

! "===== Prove that P→Q based on one piece of evidence ====="
!(synthesize (: $proof (≞ (→ P Q) $etv)) kb rb (fromNumber 1))
;; Output:
;; (: (IDIBase P2 Q2) (≞ (→ P Q) (ETV (:: (S (S Z)) ∅) (STV 0 0.5))))
;; (: (IDIBase P7 Q2) (≞ (→ P Q) (ETV (:: (S (S (S (S (S (S (S Z))))))) ∅) (STV 1 0.5))))

! "===== Prove that P→Q based on two pieces of evidence ====="
!(synthesize (: $proof (≞ (→ P Q) $etv)) kb rb (fromNumber 3))
;; Output:
;; (: (IDIRecursive P7
;;                  Q7
;;                  (IDIBase P2 Q2)
;;                  (SuccMonotonicity (SuccMonotonicity ZeroLTSucc)))
;;    (≞ (→ P Q) (ETV (:: (S (S (S (S (S (S (S Z))))))) (:: (S (S Z)) ∅)) (STV 0 0.6666666666666666))))

! "===== Prove that P→R based on one piece of evidence and deduction ====="
!(synthesize (: $proof (≞ (→ P R) $etv)) kb rb (fromNumber 3))
;; Output:
;; (: (Deduction Pm Qm Rm (EvidenceElimination (IDIBase P2 Q2)) QRm) (≞ (→ P R) (STV 0.18571428571428572 0.1)))
;; (: (Deduction Pm Qm Rm (EvidenceElimination (IDIBase P7 Q7)) QRm) (≞ (→ P R) (STV 0.9 0.1)))

;; Disabled because it takes to long (over an hour)
;; ! "===== Prove that P→R based on two pieces of evidence and deduction ====="
;; !(synthesize (: (Deduction Pm Qm Rm $PQ QRm) (≞ (→ P R) $etv)) kb rb (fromNumber 5))
