;; MeTTa port of the Implication Direct Introduction PLN rule, using
;; the match operator.
;;
;; (P a₁) ≞ p₁
;; ...
;; (P aₙ) ≞ pₙ
;; (Q a₁) ≞ q₁
;; ...
;; (Q aₙ) ≞ qₙ
;; ⊢
;; P→Q ≞ tv
;;
;; where pᵢ (resp. qᵢ) are of boolean type representing whether (P aᵢ)
;; (resp. (Q aᵢ)) has been observed as True or False, and tv
;; represents the resulting truth value of the conclusion.

;; Import modules
!(import! &self ../common/OrderedSet.metta)
!(import! &self ../common/EvidentialTruthValue.metta)
!(import! &self ../common/ImplicationDirectIntroductionFormula.metta)

;;;;;;;;;;;;;;;;;;;;;
;; Rule Definition ;;
;;;;;;;;;;;;;;;;;;;;;

;; To deal with an abritrary number of pieces of evidence the rule is
;; defined inductively.

;; Base case (axiomatic rule):
;;
;; p→q ≞ (ETV Empty (STV 1 0))
;;
;; Directly present in the atomspace.
(= (idi_axiom)
   (≞ (→ $p $q) (ETV Empty (STV 1 0))))

;; Recursive step (inductive rule):
;;
;; (p a) ≞ patv
;; (q a) ≞ qatv
;; p→q ≞ (ETV ev pqtv)
;; a ∉ ev
;; ⊢
;; p→q ≞ (ETV (insert a ev) tv)
;;
;; where tv is calculated as defined in the idi_formula.
;;
;; IDI rule as a function
(= (idi_induction (≞ ($p $a) $patv)
                  (≞ ($q $a) $qatv)
                  (≞ (→ $p $q) (ETV $ev $pqtv))
                  (∉ $a $ev))
   (≞ (→ $p $q) (ETV (insert $a $ev) (idi_formula $patv $qatv $pqtv))))

; Corresponding matching query of IDI rule
;;; There is a bug with MeTTa interpreter in using let* for this matching query, 
; as it matches ($p $a), ($q $a) with (= (idi_induction_match $kb) ...) pattern. 
; Thus the interpreter gets in aninfinite loop. The solution given by Alexey is 
; to set Type (: ≞ (-> Atom Atom Atom)), but using such Type causes the evaluations 
; inside the conclusion to don't get reduced. So, I am not using let* until gets fixed.
;;; 
(: idi_induction_match (-> $kb Atom))
(= (idi_induction_match $kb)
   (match $kb
      ;; Premises
      (, (≞ ($p $a) $patv)
         (≞ ($q $a) $qatv)
         (≞ (→ $p $q) (ETV $ev $pqtv))
         (∉ $a $ev))  
      ;; Conclusion
      (idi_induction (≞ ($p $a) $patv) (≞ ($q $a) $qatv) (≞ (→ $p $q) (ETV $ev $pqtv)) (∉ $a $ev))))
