
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 53../../../hyperon-experimental/python/tests/test_examples.py :317
; <pre>
        self.assertEqual(repr(metta.run("!('A')")), "[[('A')]]")
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 317 <br> 
 

; <pre>
!("A")
;</pre>

; <br> lastline 318 <br> 
 

; <pre>
        self.assertEqualMettaRunnerResults(metta.run("!(get-type 'A')"), [[S('Char')]])
;</pre>

; <br> lastline 319 <br> 
 

; <pre>
; 15. 20241007 R.v.Vessum converted python source to metta 
!(assertEqualToResult (get-type "A")  [[S('String' &nbsp &nbsp ) 
;</pre>

; <br>  lastline 53 <br> 
