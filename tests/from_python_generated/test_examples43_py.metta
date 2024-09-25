
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 43../../../hyperon-experimental/python/tests/test_examples.py :85
; <pre>
!((py-dot (Setter ploca 5) let))
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 85 <br> 
 

; <pre>
! ploca
;</pre>

; <br> lastline 86 <br> 
 

; <pre>
        self.assertEqual(ploca.get_object().value, 10)
;</pre>

; <br> lastline 87 <br> 
 

; <pre>
!((py-dot (SetAtom ploca 5) latom))
;</pre>

; <br> lastline 88 <br> 
 

; <pre>
! ploca
;</pre>

; <br> lastline 89 <br> 
 

; <pre>
        self.assertEqual(ploca.get_object().value, 5)
;</pre>

; <br>  lastline 95 <br> 
