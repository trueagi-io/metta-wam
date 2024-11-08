
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 51../../../hyperon-experimental/python/tests/test_examples.py :236
; <pre>

            (= (:? $c)
               (match &self (:: $c $t) $t))
            (= (:? ($c $a))
               (let $at (:? $a)
                    (match &self (:: $c (-> $at $t)) $t)))
            (= (:? ($c $a $b))
               (let* (($at (:? $a))
                      ($bt (:? $b)))
                     (match &self (:: $c (-> $at $bt $t)) $t)))

            (:: = (-> $t $t Type))

            (:: Entity Type)
            (:: Human (-> Entity Type))
            (:: Socrates Entity)
            (:: Plato Entity)
            (:: Mortal (-> Entity Type))
            (:: HumansAreMortal (-> (Human $t) (Mortal $t)))
            (:: SocratesIsHuman (Human Socrates))
            (:: SocratesIsMortal (Mortal Socrates))
        
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 257 <br> 
 

; <pre>

            !(:? (Human Plato))
            !(:? (Human Time))
            !(:? (HumansAreMortal SocratesIsHuman))
            !(:? (= SocratesIsMortal (HumansAreMortal SocratesIsHuman)))
            !(:? (= Human Entity))
            !(:? (= (Human Socrates) Plato))
            !(:? (= SocratesIsHuman SocratesIsMortal))
            !(:? (= (Human Socrates) (Human Plato)))
            !(:? (= Human Mortal))
            !(:? (= HumansAreMortal Mortal))
            
;</pre>

; <br> lastline 281 <br> 
 

; <pre>
        self.assertEqualMettaRunnerResults(metta.run("!(:? (HumansAreMortal (Human Socrates)))"), [[]])
;</pre>

; <br>  lastline 286 <br> 
