
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 93../../../hyperon-experimental/python/tests/test_load.py :15
; <pre>

            !(bind! &space (new-space))
            !(load-ascii &space {test_file})
        
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 18 <br> 
 

; <pre>
        content = metta.run("!(match &space $x $x)")[0]
;</pre>

; <br>  lastline 29 <br> 
