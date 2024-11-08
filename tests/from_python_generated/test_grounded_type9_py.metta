
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 9../../../hyperon-experimental/python/tests/test_grounded_type.py :10
; <pre>
        self.assertEqual(
            metta.parse_single("+").get_grounded_type(),
            metta.parse_single("*").get_grounded_type())
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 12 <br> 
 

; <pre>
        self.assertEqual(
            metta.run("!(+ (* 1 4) 2)")[0][0].get_grounded_type(),
            metta.parse_single("0").get_grounded_type())
;</pre>

; <br> lastline 15 <br> 
 

; <pre>
        self.assertEqual(
            metta.run("!(or True False)")[0][0].get_grounded_type(),
            metta.parse_single("False").get_grounded_type())
;</pre>

; <br> lastline 18 <br> 
 

; <pre>
        self.assertEqual(
            metta.run("!(> (* 2 2) 1)")[0][0].get_grounded_type(),
            metta.run("!(or True True)")[0][0].get_grounded_type())
;</pre>

; <br>  lastline 24 <br> 
