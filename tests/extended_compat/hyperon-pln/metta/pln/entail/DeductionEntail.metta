;; MeTTa port for deduction PLN rule
;; as the entail relationship, ⊢
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

;; Import formula functions
!(import! &self ../common/DeductionFormula.metta)

;;;;;;;;;;;;;;;;;;;;;
;; Rule Definition ;;
;;;;;;;;;;;;;;;;;;;;;

(⊢ ;; premises
   (≞ $P $ptv)
   (≞ $Q $qtv)
   (≞ $R $rtv)
   (≞ (→ $P $Q) $pqtv)
   (≞ (→ $Q $R) $qrtv)
   ;; conclusion
   (≞ (→ $P $R) (deduction-formula $ptv $qtv $rtv $pqtv $qrtv)))
