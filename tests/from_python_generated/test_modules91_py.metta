
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 91../../../hyperon-experimental/python/tests/test_modules.py :33
; <pre>
;        result = metta.run("!(match &self ($x notprime) $x)")
;</pre>

; !(match &self ($x notprime) $x)
!(assertEqualToResult ( match &self ($x notprime) $x )  ( )  )
; <br> lastline 33 <br> 
 

; <pre>
;        self.assertEqual(result[0], [S("six")])
;</pre>

; <br>  lastline 54 <br> 
