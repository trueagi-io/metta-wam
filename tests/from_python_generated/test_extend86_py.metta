
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 86../../../hyperon-experimental/python/tests/test_extend.py :53
; <pre>

              !(import! &self ext_recursive:level-2:ext_nested)
              !(get-by-key &my-dict "A")
              !(get-by-key &my-dict 6)
            
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 61 <br> 
 

; <pre>
! &runner
;</pre>

; <br>  lastline 73 <br> 
