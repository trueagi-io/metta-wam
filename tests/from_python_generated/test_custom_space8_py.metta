
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 8../../../hyperon-experimental/python/tests/test_custom_space.py :133
; <pre>
        self.assertEqual(metta.space().get_payload().test_attrib, "Test Space Payload Attrib")
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 133 <br> 
 

; <pre>
        self.assertEqualMettaRunnerResults(metta.run("!(match &self (= key $val) $val)"),
            [[S("val")]]
        )
;</pre>

; <br>  lastline 10 <br> 
