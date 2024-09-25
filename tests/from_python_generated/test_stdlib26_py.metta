
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 26../../../hyperon-experimental/python/tests/test_stdlib.py :32
; <pre>

                  ! (bind! &pow (py-atom math.pow (-> Number Number Number)))
                  ! (bind! &math (py-atom math))
        
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 35 <br> 
 

; <pre>
! (&pow 2 3)
;</pre>

; <br> lastline 37 <br> 
 

; <pre>
! ((py-dot &math floor) (py-dot &math pi))
;</pre>

; <br> lastline 39 <br> 
 

; <pre>
! ((py-dot (py-atom "{5: \'f\'}") get) 5)
;</pre>

; <br> lastline 41 <br> 
 

; <pre>
! ((py-atom str) 5)
;</pre>

; <br> lastline 43 <br> 
 

; <pre>

                ! ((py-dot (py-atom os) path.commonpath)
                   (py-atom "['/usr/lib', '/usr/local/lib']"))
        
;</pre>

; <br>  lastline 53 <br> 
