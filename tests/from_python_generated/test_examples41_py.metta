
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 41../../../hyperon-experimental/python/tests/test_examples.py :70
; <pre>
! pglob
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 70 <br> 
 

; <pre>
!((py-dot (Setter pglob 5) act))
;</pre>

; <br> lastline 71 <br> 
 

; <pre>
        self.assertEqual(pglob.get(), 5)
;</pre>

; <br> lastline 72 <br> 
 

; <pre>
! pglob
;</pre>

; <br> lastline 73 <br> 
 

; <pre>
!((py-dot (Setter ploc 5) let))
;</pre>

; <br> lastline 74 <br> 
 

; <pre>
        self.assertEqual(ploc, 10)
;</pre>

; <br> lastline 75 <br> 
 

; <pre>
! ploc
;</pre>

; <br>  lastline 81 <br> 
