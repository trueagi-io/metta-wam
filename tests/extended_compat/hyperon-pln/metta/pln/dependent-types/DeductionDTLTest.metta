;; ;; Type definitions checker
;; !(pragma! type-check auto)

;; ;; Equality type definition
;; (: = (-> $t $t Type))

;; Import modules
!(import! &self DeductionDTL.metta)
!(import! &self ../../synthesis/Synthesize.metta)
!(import! &self ../../common/Record.metta)

;; Knowledge base
(: Predicate Type)
(: → (-> Predicate Predicate Type))
(: P Predicate)
(: Q Predicate)
(: R Predicate)
(: kb (-> Atom))
(= (kb) (superpose ((: Pm (≞ P (STV 0.5 0.1)))
                    (: Qm (≞ Q (STV 0.6 0.2)))
                    (: Rm (≞ R (STV 0.7 0.3)))
                    (: PQm (≞ (→ P Q) (STV 0.8 0.4)))
                    (: QRm (≞ (→ Q R) (STV 0.9 0.5))))))

;; Rule base
(: rb (-> Atom))
(= (rb) (deduction-rule))

;; Test deduction rule
;;
;; Build the following inference tree
;;
;; ------------(Pm)  ----------(Qm)  ------------(Rm)  ------------  --(PQm)  ----------------(QRm)
;; P ≞ <0.5 0.1>     Q ≞ <0.6 0.2>   R ≞ <0.7 0.1>     P → Q ≞ <0.8 0.5>      Q → R ≞ <0.9 0.5>
;; -------------------------------------------------------------------------------------------(Deduction)
;;                                          P → R ≞ $tv

;; The following is disabled till the type checker supports running functions inside types
;; ! "===== Test type checker on deduction rule ====="
;; (: deduction_prf (≞ (→ P R) $tv))
;; (= deduction_prf (Deduction Pm Qm Rm PQm QRm))
;; !(deduction_prf)
;; !(get-type deduction_prf)

;; Test synthesizer
! "===== Test synthesizer ===="
!(assertEqual (synthesize (: $proof (≞ (→ P Q) $tv)) kb rb Z) (: PQm (≞ (→ P Q) (STV 0.8 0.4))))
!(assertEqual (synthesize (: $proof (≞ (→ Q R) $tv)) kb rb Z) (: QRm (≞ (→ Q R) (STV 0.9 0.5))))
;; Do not use assert because of all the incompleted reductions
!(synthesize (: $proof (≞ (→ P R) $tv)) kb rb (S Z)) ; (: (Deduction Pm Qm Rm PQm QRm) (≞ (→ P R) (STV 0.8 0.1))))

;; TODO: fix the following
;;
;; SOLUTION: do partial evaluation where each substitution is
;; provable.  For instance: arguments are well typed for function
;; calls, conditions are proven to be true or false for conditionals,
;; etc.
;;
;; !(record synthesize ((: $proof (≞ (→ P R) (STV $s $c))) kb rb (S Z))) ; (: (Deduction Pm Qm Rm PQm QRm) (≞ (→ P R) (STV 0.8 0.1)))
