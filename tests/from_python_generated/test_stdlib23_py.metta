
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 23../../../hyperon-experimental/python/tests/test_stdlib.py :10
; <pre>
 ;       self.assertEqualMettaRunnerResults(metta.run("!(repr (my atom))"),
 ;                                          [[ValueAtom("(my atom)")]])
;</pre>

; ready works 
!(assertEqualToResult ( repr (my atom) )  ( "(my atom)" )  )

; <br> lastline 11 <br> 
; <pre>
;        self.assertEqualMettaRunnerResults(metta.run("!(parse \"(my atom)\")"),
;                                           [[E(S("my"), S("atom"))]])
;</pre>

; <br>  lastline 17 <br> 
