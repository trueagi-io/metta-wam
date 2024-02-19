;; MeTTa port of the Implication Direct Introduction PLN rule, encoded
;; as a non-determinist equality, =
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
(≞ (→ $p $q) (ETV Empty (STV 1 0)))

;; TODO: see MeTTa examples such as b2_backchain.metta,
;; b3_direct.metta, b4_nonterm.metta, c3_pln_stv.metta,
;; d1_custom_types.metta and d3_typed_pln.metta for inspiration.

;; Recursive step (inductive rule):
;;
;; (p a) ≞ patv
;; (q a) ≞ qatv
;; p→q ≞ (ETV ev pqtv)
;; a ∉ ev
;; ⊢
;; p→q ≞ (ETV (insert a ev) tv)
;;
;; where tv is calculated by idi_formula defined in
;; ImplicationDirectIntroductionFormula.metta.
;;
;; Two versions are defined, forward and backward.

;; Backward: this could be what we want, the non-determinism allows to
;; have the same left hand side expression being reached through
;; multiple paths.
;;
;; Unfortunately, the interpreter cannot unify the left hand side of
;; the equality because it contains function calls, as opposed to just
;; constructors.
(= (≞ (→ $p $q) (ETV (insert $a $ev) (idi_formula $patv $qatv $pqtv)))
   (, (≞ ($p $a) $patv)
      (≞ ($q $a) $qatv)
      (≞ (→ $p $q) (ETV $ev $pqtv))
      (∉ $a $ev)))

;; Forward: in that case the multiplicity of paths is obtained by
;; having multiple path pointing to the same right hand side
;; expression.
(= (, (≞ ($p $a) $patv)
      (≞ ($q $a) $qatv)
      (≞ (→ $p $q) (ETV $ev $pqtv))
      (∉ $a $ev))
    (≞ (→ $p $q) (ETV (insert $a $ev) (idi_formula $patv $qatv $pqtv))))
