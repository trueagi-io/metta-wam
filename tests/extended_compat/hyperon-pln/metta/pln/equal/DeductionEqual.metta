;; MeTTa port for deduction PLN rule
;; using equality, =
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

;; Forward
(= (, (≞ $p $ptv)
      (≞ $q $qtv)
      (≞ $r $rtv)
      (≞ (→ $p $q) $pqtv)
      (≞ (→ $q $r) $qrtv))
    (≞ (→ $p $r) (deduction-formula ($ptv $qtv $rtv $pqtv $qrtv))))

;; Backward
;; Unfortunately this won't work, the interpreter cannot unify the left hand side of
;; the equality because it contains function calls, as opposed to just constructors.

(= (≞ (→ $p $r) (deduction-formula $ptv $qtv $rtv $pqtv $qrtv))
   (, (≞ $p $ptv)
      (≞ $q $qtv)
      (≞ $r $rtv)
      (≞ (→ $p $q) $pqtv)
      (≞ (→ $q $r) $qrtv)))
