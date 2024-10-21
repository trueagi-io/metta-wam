; 20241007 R.v.Vessum converted python source to metta 
; FILE: 47../../../hyperon-experimental/python/tests/test_examples.py :126
;
(= (eq $x $x) True)
(= (plus Z $y) $y)
(= (plus (S $k) $y) (S (plus $k $y)) )
;  !(assertEqualToResult ( )  (  )  )
; <br> lastline 130 <br> 
; !(eq (+ 2 2) 4)
; !(eq (+ 2 3) 4)
; !(eq (plus Z $n) $n)

; orginal python 
; output = metta.run('!(eq (plus (S Z) $n) $n)')
; self.assertAtomsAreEquivalent(output[0], metta.parse_all('(eq (S $y) $y)'))
; 
;!(plus (S Z) p) ; returns (S p)
;!(plus (S Z) $n) ; returns (S $n)

;!(eq (plus (S Z) $n) $n )
;!(eq (S $y) $y )

; !(assertEqualToResult (eq (plus (S Z) $n) $n )  (eq (S $y) $y )  )
; !(assertEqualToResult (eq (plus (S Z) $n) $n )  ( True )  )
!(assertEqualToResult ( eq (S $y) $y )  ( False )  )
 


; <br>  lastline 161 <br> 
