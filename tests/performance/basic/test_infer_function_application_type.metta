
(= (if True $then) $then)

(= (: (apply $f $x) $r) (and (: $f (=> $a $r)) (: $x $a)))
(= (: reverse (=> String String)) True)
(= (: "Hello" String) True)

!(assertEqual (if (: (apply reverse "Hello") $t) $t) String)


(= (:? $c)
(match &self (:= $c $t) $t))
(= (:? ($c $a))
(match &self (:= ($c (:? $a)) $t) $t))
(= (:? ($c $a $b))
(match &self (:= ($c (:? $a) (:? $b)) $t) $t))

(= (:check $c $t)
(match &self (:= $c $t) T))
(= (:check ($c $a) $t)
(match &self (:= ($c (:? $a)) $t) T))
(= (:check ($c $a $b) $t)
(match &self (:= ($c (:? $a) (:? $b)) $t) T))

(:= (= $t $t) Prop)

(:= Entity Prop)
(:= (Human Entity) Prop)
(:= Socrates Entity)
(:= Plato Entity)
(:= Time NotEntity)
(:= (Mortal Entity) Prop)
(:= (HumansAreMortal (Human $t)) (Mortal $t))
(:= SocratesIsHuman (Human Socrates))
(:= SocratesIsMortal (Mortal Socrates))

(:= Sam Entity)
(:= (Frog Entity) Prop)
(:= (Green Entity) Prop)
(:= (Croaks Entity) Prop)
(:= (GreenAndCroaksIsFrog (Green $t) (Croaks $t)) (Frog $t))
(:= SamIsGreen (Green Sam))
(:= SamCroaks (Croaks Sam))

!(assertEqual (:? (HumansAreMortal SocratesIsHuman)) (Mortal Socrates))
!(assertEqual (:check (HumansAreMortal SocratesIsHuman) (Mortal Socrates)) T)
!(assertEqual (:? (= SocratesIsMortal (HumansAreMortal SocratesIsHuman))) Prop)
!(assertEqual (:check (= (Mortal Plato) (Mortal Socrates)) Prop) T)
!(assertEqual (:check (= (Human Socrates) (Mortal Socrates)) Prop) T) ; they are both of Prop type and can be equated
!(assertEqual (:? (GreenAndCroaksIsFrog SamIsGreen SamCroaks)) (Frog Sam))
; some negative examples
!(assertEqual (:check (= SocratesIsHuman SocratesIsMortal) Prop))
!(assertEqual (:? (SocratesIsHuman (Human Socrates))))
!(assertEqual (:? (Human Time)))
; The following doesn't work: `(:? (HumansAreMortal (Human Time)))` is matched against `(:? $c)`'
; Then, `(:= $c $t)` is matched against `(:= (HumansAreMortal (Human $t)) (Mortal $t))`
; immediately resulting in `(Mortal Time)`. Thus, it doesn't matter that matching against
; `(= (:? ($c $a))` doesn't work (since `(:? $a)` is incorrect).
; !(assertEqual (:? (HumansAreMortal (Human Time)))
; It should be noted that `(HumansAreMortal (Human Socrates))` is also an incorrectly typed
; expression, since HumansAreMortal expects an element of (Human Socrates) - not the type itself

; Another syntax

(= (:? $c)
(match &self (:: $c $t) $t))
(= (:? ($c $a))
(match &self (:: $c (-> (:? $a) $t)) $t))
(= (:? ($c $a $b))
(match &self (:: $c (-> (:? $a) (:? $b) $t)) $t))

(:: = (-> $t $t Type))

(:: Entity Type)
(:: Human (-> Entity Type))
(:: Socrates Entity)
(:: Plato Entity)
(:: Mortal (-> Entity Type))
(:: HumansAreMortal (-> (Human $t) (Mortal $t)))
(:: SocratesIsHuman (Human Socrates))
(:: SocratesIsMortal (Mortal Socrates))

; :? just infers the type of the expression - not its inhabitance
!(assertEqual (:? (Human Plato)) Type)
!(assertEqual (:? (Human Time)))
!(assertEqual (:? (HumansAreMortal SocratesIsHuman)) (Mortal Socrates))
!(assertEqual (:? (= SocratesIsMortal (HumansAreMortal SocratesIsHuman))) Type)
!(assertEqual (:? (= Human Entity)))
!(assertEqual (:? (= (Human Socrates) Plato)))
!(assertEqual (:? (= SocratesIsHuman SocratesIsMortal)))

; `(Human Socrates)` and `(Human Plato)` are different types, but they are elements
; of the same Type, so they can be equated
!(assertEqual (:? (= (Human Socrates) (Human Plato))) Type)
!(assertEqual (:? (= Human Mortal)) Type)
!(assertEqual (:? (= HumanAreMortal Mortal)))

; Interestingly, the following example works correctly in this syntax, because
; application `(Human Socrates)` is not mixed up with dependent type definition
!(assertEqual (:? (HumansAreMortal (Human Socrates))))



