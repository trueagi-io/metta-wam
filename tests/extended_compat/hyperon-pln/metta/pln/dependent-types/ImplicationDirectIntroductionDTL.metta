;; MeTTa port of the Implication Direct Introduction PLN rule, encoded
;; as dependent types, i.e. propositions as types.
;;
;; See ImplicationDirectIntroductionEntail.metta for more detail on a
;; pseudo formal description.
;;
;; In the case of dependent types, inference rules are constructors of
;; the propositions to be proven.  Then a proof is an tree composed of
;; calls of such constructors.
;;
;; For instance assuming two inference rules
;;
;; ImplicationDirectIntroduction (IDI, for short)
;; Deduction (DED, for short)
;;
;; The following inference tree
;;
;; ...            ...
;; -----(IDI)     -----(IDI)
;; P → Q          Q → R
;; ---------------------(DED)
;;         P → R
;;
;; is represented by the following MeTTa expression
;;
;; (Deduction
;;    (ImplicationDirectIntroduction ...)
;;    (ImplicationDirectIntroduction ...))
;;
;; where the premises of ImplicationDirectIntroduction are left
;; undefined for now.
;;
;; How such propositions as types can be defined still remains to be
;; determined but ideally it may look something like
;;
;; (: Deduction
;;    (-> (≞ (→ $P $Q) $TV1)
;;        (≞ (→ $Q $R) $TV2)
;;        (≞ (→ $P $R) (deduction-formula $TV1 $TV2))))
;;
;; where ≞, → are dependent types, and formula is just a regular metta
;; function.

;; Import modules
!(import! &self ../../common/truthvalue/EvidentialTruthValue.metta)
!(import! &self ../../common/formula/ImplicationDirectIntroductionFormula.metta)

;;;;;;;;;;;;;;;;;;;;;
;; Rule Definition ;;
;;;;;;;;;;;;;;;;;;;;;

;; All rules are implemented as constructors of propositions encoded
;; as types.

;; To deal with an abritrary number of pieces of evidence the rule is
;; defined inductively.

;; Base case (one piece of evidence):
;;
;; (p a) = True
;; (q a) = qa
;; ⊢
;; p→q ≞ (ETV (:: a ∅) tv)
;;
;; where tv is calculated according to the direct introduction
;; formula.  Note that we don't bother to take into account pieces of
;; evidence that is not True on (p a), because it has no impact on the
;; resulting truth value otherwise.
;;
;; To avoid prematurely reducing (p a) = pa to pa = pa, the following
;; representation is used instead
;;
;; (⊷ p a pa)
;;
;; which can be read as "under p, the image of a is pa".
;;
;; ⊷ is essentially the Atomese equivalent of ExecutionLink.  It has
;; description: IMAGE OF, hexadecimal code: 0x22B7, unicode-math code:
;; \imageof and latex code: \multimapdotbothB.
(: implication-direct-introduction-base-rule (-> Atom))
(= (implication-direct-introduction-base-rule)
   (: IDIBase
      (-> (⊷ $p $a True)
          (⊷ $q $a $qa)
          (≞ (→ $p $q)
             (ETV (:: $a ∅)
                  (direct-introduction-base-formula $qa))))))

;; Recursive step (more than one piece of evidence):
;;
;; (p a) = True
;; (q a) = qa
;; p→q ≞ (ETV (:: h t) pqtv)
;; h < a
;; ⊢
;; p→q ≞ (ETV (:: a (:: h t)) tv)
;;
;; where tv is calculated using the direct introduction formula.  Note
;; that we don't bother to take into account pieces of evidence that
;; is not True on (p a), because it has no impact on the resulting
;; truth value otherwise.
;;
;; To avoid prematurely reducing (p a) = pa to pa = pa, the following
;; representation is used instead
;;
;; (⊷ p a pa)
;;
;; which can be read as "under p, the image of a is pa".
;;
;; For a similar reason, the premise h < a is represented as
;;
;; h ⍃ a
;;
;; ⍃ is the axiomatized version of <.  It is defined in Num.metta.
;; The box around the less than sign is here to manifest its
;; axiomatic, as opposed to computational, nature.  It has
;; description: APL FUNCTIONAL SYMBOL QUAD LESS-THAN and hexadecimal
;; code: 0x2343.
;;
;; The h ⍃ a premise is there to garanty that evidence is not double
;; counted.
;;
;; Historical note: initially the premise
;;
;; h ⍃ a
;;
;; was replaced by
;;
;; a ∉ (:: h t)
;;
;; But then it was realized that it is far more efficient to enforce
;; the order in which evidence can be pilled up, than to prove that a
;; new piece of evidence does not belongs to an existing evidence set.
;; And now you know why time is the way it is.
(: implication-direct-introduction-recursive-rule (-> Atom))
(= (implication-direct-introduction-recursive-rule)
   (: IDIRecursive
      (-> (⊷ $p $a True)
          (⊷ $q $a $qa)
          (≞ (→ $p $q) (ETV (:: $h $t) $pqtv))
          (⍃ $h $a)
          (≞ (→ $p $q)
             (ETV (:: $a (:: $h $t))
                  (direct-introduction-recursive-formula $qa $pqtv))))))

;; Convert an EvidenceTruthValue to a TruthValue (i.e. elimate the
;; evidential part).
;;
;; (ETV evidence tv)
;; ⊢
;; tv
(: evidence-elimination-rule (-> Atom))
(= (evidence-elimination-rule)
   (: EvidenceElimination
      (-> (≞ $a (ETV $evidence $tv))
          (≞ $a $tv))))
