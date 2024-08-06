;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Types as propositions can be used in MeTTa but with nuances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Entities
(: Entity Type)
(: Socrates Entity)
(: Plato Entity)

; Predicates (functions) from `Entity` to `Type`
(: Human (-> Entity Type))
(: Mortal (-> Entity Type))

;Plato is Mortal proposition
(Mortal Plato)

; A function type as an implication
; `HumansAreMortal` represents the implication:
; "For all x, if Human(x) then Mortal(x)"
(: HumansAreMortal (-> (Human $t) (Mortal $t)))

; These might be considered as assertions that Plato and
; Socrates are Humans:
(: SocratesIsHuman (Human Socrates))
(: PlatoIsHuman (Human Plato))

!(get-type (HumansAreMortal SocratesIsHuman))

(: SocratesIsMortal (Mortal Socrates))
!(get-type SocratesIsMortal)
!(get-type (Human Socrates))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Equality can be typed. If some equality is well-typed,
; it means that it can be true.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: = (-> $t $t Type))
!(get-type (= SocratesIsMortal (HumansAreMortal SocratesIsHuman)))
!(get-type (= SocratesIsMortal (HumansAreMortal PlatoIsHuman))) ;empty

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Another (MeTTa) way is to have explicit reduction of types to True
; (equalities over equalities), although a more practical way would
; be just to use a dedicated comparison operator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: T Type)
(= (= $x $x) T)

(= (HumansAreMortal SocratesIsHuman) SocratesIsMortal)

; !(= SocratesIsMortal (HumansAreMortal SocratesIsHuman))
!(= SocratesIsMortal (HumansAreMortal SocratesIsHuman))
!(= (+ 1 2) (+ 3 0))

; One can also define an equality for directly inhabited types
(= (= $type T)
    (match &self (: $x $type) T))

!(= (Mortal Socrates) T); returns T because it is inhabited
!(= (Mortal Plato) T) ;empty, because it is not inhabited
