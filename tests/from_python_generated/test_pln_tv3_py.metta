
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 3../../../hyperon-experimental/python/tests/test_pln_tv.py :10
; <pre>

    (= (min $a $b) (if (< $a $b) $a $b))
    (= (s-tv (stv $s $c)) $s)
    (= (c-tv (stv $s $c)) $c)
    (= (stv (And $a $b))
   (stv (min (s-tv (stv $a)) (s-tv (stv $b)))
    (min (c-tv (stv $a)) (c-tv (stv $b)))))
    (= (stv (P A)) (stv 0.5 0.8))
    (= (stv (P B)) (stv 0.3 0.9))
        
;</pre>
;  !(assertEqualToResult ( )  (  )  )
; <br> lastline 19 <br> 
; <pre>
        (: A Concept)
        (: B Concept)
        (: P Predicate)
        
;</pre>
; <br> lastline 24 <br> 
; <pre>
; 3. 20241007 R.v.Vessum converted python source to metta 
;ready works 
!(assertEqualToResult (stv (And (P A) (P B))) ((stv 0.3 0.8))  ) 

;</pre>
; <br> lastline 27 <br> 
; <pre>
;</pre>
; <br> lastline 30 <br> 
; <pre>
; 4. 20241007 R.v.Vessum converted python source to metta 

; ready works 
(= (pln $expr) ($expr (stv $expr)))
!(assertEqualToResult (pln (And (P A) (P $x)))  ((And (P A) (P A)) (stv 0.5 0.8))                 ((And (P A) (P B)) (stv 0.3 0.8))              &nbsp &nbsp ) 


;</pre>
; <br>  lastline 12 <br> 
