;; MeTTa port for modus ponens PLN rule
;; as DTL (Dependent Type Language)
;;
;; (≞ A atv)
;; (≞ (→ A B) abtv)
;; ⊢
;; (≞ B btv)
;;
;; where atv and abtv are truth values of the premises A and (→ A B) respectively,
;; and btv represents the resulting truth value of the conclusion.
;;
;; This rule contains less premises but thus is less precise.
;; In order to properly calculate P(B) it must
;; account for P(B|A) and P(B|Not A), as follows
;;
;; P(B) = P(B|A)*P(A) + P(B|Not A)*P(Not A)
;;
;; However in this rule to avoid requiring too many premises it is
;; assumed
;;
;; P(B|Not A) = 0.2
;;
;; which is a temporary hack. Maybe the resulting confidence could at
;; least be degraded to conpensate for that hack. But ideally one
;; should use a precise modus ponens (requiring P(B|Not A) as
;; premises), and then if missing, these premises could be estimated
;; via other rules.


;; Import modules
!(import! &self ../../common/truthvalue/MeasEq.metta)
!(import! &self ../../common/formula/ModusPonensFormula.metta)

;; Define modus ponens rule
(: modus-ponens-rule (-> Atom))
(= (modus-ponens-rule)
   (: ModusPonens
      (-> (≞ $a $atv)
          (≞ (→ $a $b) $abtv)
          (≞ $b (modus-ponens-formula $atv $abtv)))))
