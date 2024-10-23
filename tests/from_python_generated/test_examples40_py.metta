
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 40../../../hyperon-experimental/python/tests/test_examples.py :44
; <pre>

            (= (remove-st $var)
               (match &self (state $var $y)
                  (remove-atom &self (state $var $y))))

            (= (change-st $var $value)
               (superpose ((remove-st $var)
                  (add-atom &self (state $var $value)))
               )

                )

;            (= (change-st $var $value)
;                ((remove-st $var)
;                  (add-atom &self (state $var $value)))  )

            (= (get-st $var)
               (match &self (state $var $value) $value))
 
!(change-st (name id-001) Fritz)
; !(remove-st (name id-001) )
;!(remove-st id-001 )

; ready works
;</pre>
!(assertEqualToResult (get-st (name id-001) )  ( Fritz )  )
; (get-st (name id-001))      [[S('Fritz' &nbsp &nbsp ) 
; <br> lastline 54 <br> 
 

; <pre>
;!(change-st (name id-001) Fritz)
;</pre>

; <br> lastline 55 <br> 
 

; <pre>
; 11. 20241007 R.v.Vessum converted python source to metta 
;!(assertEqualToResult (get-st (name id-001))                           [[S('Fritz' &nbsp &nbsp ) 
;</pre>

; <br> lastline 57 <br> 
 

; <pre>
;!(change-st (name id-001) Sam)
;</pre>

; <br> lastline 58 <br> 
 

; <pre>
; 12. 20241007 R.v.Vessum converted python source to metta 
;!(assertEqualToResult (get-st (name id-001))                           [[S('Sam' &nbsp &nbsp ) 
;</pre>

; <br>  lastline 70 <br> 
