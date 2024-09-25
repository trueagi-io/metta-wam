
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 29../../../hyperon-experimental/python/tests/test_stdlib.py :72
; <pre>
!(py-list ())
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 72 <br> 
 

; <pre>
!(py-tuple ())
;</pre>

; <br> lastline 73 <br> 
 

; <pre>
!(py-dict ())
;</pre>

; <br> lastline 74 <br> 
 

; <pre>
!(py-tuple (1 (2 (3 "3")) (py-atom list)))
;</pre>

; <br> lastline 75 <br> 
 

; <pre>
!(py-list (1 2 (4.5 3)))
;</pre>

; <br> lastline 76 <br> 
 

; <pre>
!(py-list (1 2 (py-tuple (3 4))))
;</pre>

; <br> lastline 77 <br> 
 

; <pre>
!(py-dict ((a "b") ("b" "c")))
;</pre>

; <br> lastline 79 <br> 
 

; <pre>
!(py-list (a b c))
;</pre>

; <br> lastline 81 <br> 
 

; <pre>
!(py-chain (1 2 3 4))
;</pre>

; <br> lastline 83 <br> 
 

; <pre>
!(py-dict (("a" "b" "c") ("b" "c")))
;</pre>

; <br>  lastline 88 <br> 
