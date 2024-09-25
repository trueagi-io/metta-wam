
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 27../../../hyperon-experimental/python/tests/test_stdlib.py :53
; <pre>
        self.assertEqualMettaRunnerResults(metta.run("!(+ 1 2)"), [[ValueAtom(3)]])
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 53 <br> 
 

; <pre>
        self.assertEqualMettaRunnerResults(metta.run("!(+ 5.0 -2.0)"), [[ValueAtom(3.0)]])
;</pre>

; <br> lastline 54 <br> 
 

; <pre>
        self.assertEqualMettaRunnerResults(metta.run("!(+ 1.0e3 2.0e3)"), [[ValueAtom(3e3)]])
;</pre>

; <br> lastline 55 <br> 
 

; <pre>
        self.assertEqualMettaRunnerResults(metta.run("!(+ 5e-3 -2e-3)"), [[ValueAtom(3e-3)]])
;</pre>

; <br>  lastline 60 <br> 
