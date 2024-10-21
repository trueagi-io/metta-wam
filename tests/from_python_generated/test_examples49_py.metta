
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 49../../../hyperon-experimental/python/tests/test_examples.py :170
; <pre>

            (= (:? $c)
               (match &self (:= $c $t) $t))
            (= (:? ($c $a))
               (let $at (:? $a)
                    (match &self (:= ($c $at) $t) $t)))
            (= (:? ($c $a $b))
               (let* (($at (:? $a))
                      ($bt (:? $b)))
                     (match &self (:= ($c $at $bt) $t) $t)))

            (= (:check $c $t)
               (match &self (:= $c $t) T))
            (= (:check ($c $a) $t)
               (let $at (:? $a)
                    (match &self (:= ($c $at) $t) T)))
            (= (:check ($c $a $b) $t)
               (let* (($at (:? $a))
                      ($bt (:? $b)))
                     (match &self (:= ($c $at $bt) $t) T)))

            (:= (= $t $t) Prop)

            (:= Entity Prop)
            (:= (Human Entity) Prop)
            (:= Socrates Entity)

;</pre>
;  !(assertEqualToResult ( )  (  )  )
; <br>  lastline 211 <br> 
;            !(:? (HumansAreMortal SocratesIsHuman))

;            !(:check (HumansAreMortal SocratesIsHuman) (Mortal Socrates))
;            !(:? (= SocratesIsMortal (HumansAreMortal SocratesIsHuman)))
            !(:check (= (Mortal Plato) (Mortal Socrates)) Prop)
;            !(:check (= (Human Socrates) (Mortal Socrates)) Prop)
;            !(:? (GreenAndCroaksIsFrog SamIsGreen SamCroaks))

;            !(:check (= SocratesIsHuman SocratesIsMortal) Prop)
;            !(:? (SocratesIsHuman (Human Socrates)))
;            !(:? (Human Time))


