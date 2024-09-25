
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 47../../../hyperon-experimental/python/tests/test_examples.py :126
; <pre>

           (= (eq $x $x) True)
           (= (plus Z $y) $y)
           (= (plus (S $k) $y) (S (plus $k $y)))
        
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 130 <br> 
 

; <pre>

            !(eq (+ 2 2) 4)
            !(eq (+ 2 3) 4)
            !(eq (plus Z $n) $n)
            
;</pre>

; <br> lastline 141 <br> 
 

; <pre>
!(eq (plus (S Z) $n) $n)
;</pre>

; <br>  lastline 161 <br> 
