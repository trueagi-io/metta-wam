
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 28../../../hyperon-experimental/python/tests/test_stdlib.py :60
; <pre>
(= (intent regex:"^Hello[[\.|!]]?$") (Intent Hello))
                    (= (intent regex:"Good~morning.*[[\\.|!]]?") (Intent Hello))
                    (= (intent $x) (empty))
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 62 <br> 
 

; <pre>
        self.assertEqual(metta.run('!(intent "hello")', True), [E(S("Intent"), S("Hello"))])
;</pre>

; <br> lastline 63 <br> 
 

; <pre>
        self.assertEqual(metta.run('!(intent "Good morning my friend!")', True),
                                           [E(S("Intent"), S("Hello"))])
;</pre>

; <br> lastline 66 <br> 
 

; <pre>
        self.assertEqual(metta.run('!(intent "Hi")', True), [])
;</pre>

; <br>  lastline 72 <br> 
