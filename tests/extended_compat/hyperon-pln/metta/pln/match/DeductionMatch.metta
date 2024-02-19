;; MeTTa port for deduction PLN rule
;; using match
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

;; Deduction rule represented as a function.
;;
;; P ≞ PTV
;; Q ≞ QTV
;; R ≞ RTV
;; P → Q ≞ PQTV
;; Q → R ≞ RQTV
;; ⊢
;; P → R ≞ TV
(= (deduction (≞ $p $ptv)
              (≞ $q $qtv)
              (≞ $r $rtv)
              (≞ (→ $p $q) $pqtv)
              (≞ (→ $q $r) $qrtv))
   (≞ (→ $p $r) (ded_formula $ptv $qtv $rtv $pqtv $qrtv)))

;; Corresponding matching query of that rule
(: deduction_match (-> $kb Atom))
(= (deduction_match $kb)
   (let* (($prem1 (≞ $p $ptv))
          ($prem2 (≞ $q $qtv))
          ($prem3 (≞ $r $rtv))
          ($prem4 (≞ (→ $p $q) $pqtv))
          ($prem5 (≞ (→ $q $r) $qrtv)))
     (match $kb
       ;; Premises
       (, $prem1 $prem2 $prem3 $prem4 $prem5)
       ;; Conclusion
       (deduction $prem1 $prem2 $prem3 $prem4 $prem5))))
