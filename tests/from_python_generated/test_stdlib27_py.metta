
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 27../../../hyperon-experimental/python/tests/test_stdlib.py :53
; <pre>
; ("!(+ 1 2)"), 3
;</pre>
; ready works 
 !(assertEqualToResult ( + 1 2 )  ( 3 )  )
; <br> lastline 53 <br> 
; ready works 
!(assertEqualToResult ( + 5.0 -2.0 )  ( 3.0 )  )
; <br> lastline 54 <br> 
; ready works 
!(assertEqualToResult ( + 1.0e3 2.0e3 )  ( 3e3 )  )
;!(+ 1.0e3 2.0e3)  [[ValueAtom(3e3)]])

; <br> lastline 55 <br> 

; ready works 
!(assertEqualToResult (+ 5e-3 -2e-3 )  ( 3e-3 )  )
; !(+ 5e-3 -2e-3) 3e-3


; <br>  lastline 60 <br> 
