;; MeTTa port for deduction PLN rule
;; as DTL (Dependent Type Language)
;;
;; (≞ P ptv)
;; (≞ Q qtv)
;; (≞ R rtv)
;; (≞ (→ P Q) tv1)
;; (≞ (→ Q R) tv2)
;; ⊢
;; (≞ (→ P R) TV)
;;
;; where ptv, qtv, rtv, tv1 and tv2 are truth values of the
;; premises P, Q, R, (→ P Q) and (→ Q R) respectively.
;; TV represents the resulting truth value of the conclusion.

;; Import modules
!(import! &self ../../common/truthvalue/MeasEq.metta)
!(import! &self ../../common/formula/DeductionFormula.metta)

;; Define deduction rule
(: deduction-rule (-> Atom))
(= (deduction-rule)
   (: Deduction
      (-> (≞ $p $ptv)
          (≞ $q $qtv)
          (≞ $r $rtv)
          (≞ (→ $p $q) $pqtv)
          (≞ (→ $q $r) $qrtv)
          (≞ (→ $p $r) (deduction-formula $ptv $qtv $rtv $pqtv $qrtv)))))
