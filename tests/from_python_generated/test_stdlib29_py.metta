
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 29../../../hyperon-experimental/python/tests/test_stdlib.py :72
; <pre>
!(py-list ())
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 72 <br> 
!(py-tuple ())
; <br> lastline 73 <br> 
; !(py-dict ())
; <br> lastline 74 <br> 
!(py-tuple (1 (2 (3 "3")) (py-atom list)))
; <br> lastline 75 <br> 
!(py-list (1 2 (4.5 3)))
; <br> lastline 76 <br> 
!(py-list (1 2 (py-tuple (3 4))))
; <br> lastline 77 <br> 
!(py-dict ((a "b") ("b" "c")))
; <br> lastline 79 <br> 
!(py-list (a b c))
; <br> lastline 81 <br> 
!(py-chain (1 2 3 4))
; <br> lastline 83 <br> 
!(py-dict (("a" "b" "c") ("b" "c")))
; <br>  lastline 88 <br> 
