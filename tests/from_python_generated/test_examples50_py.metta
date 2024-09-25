
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 50../../../hyperon-experimental/python/tests/test_examples.py :211
; <pre>

            !(:? (HumansAreMortal SocratesIsHuman))
            !(:check (HumansAreMortal SocratesIsHuman) (Mortal Socrates))
            !(:? (= SocratesIsMortal (HumansAreMortal SocratesIsHuman)))
            !(:check (= (Mortal Plato) (Mortal Socrates)) Prop)
            !(:check (= (Human Socrates) (Mortal Socrates)) Prop)
            !(:? (GreenAndCroaksIsFrog SamIsGreen SamCroaks))
            
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 226 <br> 
 

; <pre>

            !(:check (= SocratesIsHuman SocratesIsMortal) Prop)
            !(:? (SocratesIsHuman (Human Socrates)))
            !(:? (Human Time))
            
;</pre>

; <br>  lastline 236 <br> 
