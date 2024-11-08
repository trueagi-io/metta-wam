
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 12../../../hyperon-experimental/python/tests/test_grounded_type.py :49
; <pre>
        v1 = metta.run("!(id_num (+ 2 2))")[0]
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 49 <br> 
 

; <pre>
        v2 = metta.run("! 4")[0]
;</pre>

; <br> lastline 50 <br> 
 

; <pre>
        v3 = metta.run("!(as_int (+ 2 2))")[0]
;</pre>

; <br> lastline 51 <br> 
 

; <pre>
        self.assertEqual(v1, v2)
;</pre>

; <br> lastline 52 <br> 
 

; <pre>
        self.assertEqual(v1[0].get_grounded_type(), v2[0].get_grounded_type())
;</pre>

; <br> lastline 53 <br> 
 

; <pre>
        self.assertEqual(metta.run("!(id_num untyp)"), [metta.parse_all("(id_num untyp)")])
;</pre>

; <br> lastline 55 <br> 
 

; <pre>
        metta.run("(: myAtom myType)")
;</pre>

; <br> lastline 56 <br> 
 

; <pre>

            !(id_num myAtom)
            !(id_num False)
            
;</pre>

; <br>  lastline 64 <br> 
