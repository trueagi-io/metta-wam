
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 87../../../hyperon-experimental/python/tests/test_extend.py :73
; <pre>
        self.assertEqual(g_object, None)
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 73 <br> 
 

; <pre>

          !(import! &self extension)
          !(set-global! 42)
        
;</pre>

; <br> lastline 78 <br> 
 

; <pre>
!(get-global)
;</pre>

; <br> lastline 81 <br> 
 

; <pre>
        self.assertEqual(g_object, 42)
;</pre>

; <br>  lastline 92 <br> 
