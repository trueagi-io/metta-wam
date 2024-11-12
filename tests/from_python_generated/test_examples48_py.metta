
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 48../../../hyperon-experimental/python/tests/test_examples.py :161
; <pre>
        self.assertEqualMettaRunnerResults(metta1.run('!(inverse B)'), [[S('A')]])
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 161 <br> 
 

; <pre>
        self.assertEqualMettaRunnerResults(metta2.run('!(find-in &space1 B)'), [[S('A')]])
;</pre>

; <br> lastline 162 <br> 
 

; <pre>
        self.assertEqualMettaRunnerResults(metta2.run('!(find-in &self B)'), [[S('C')]])
;</pre>

; <br> lastline 163 <br> 
 

; <pre>
        self.assertEqualMettaRunnerResults(metta2.run('!(borrow &space1 (inverse B))'), [[S('A')]])
;</pre>

; <br> lastline 164 <br> 
 

; <pre>
        self.assertEqualMettaRunnerResults(metta2.run('!(borrow &space1 (how-it-works?))'), [[S('success')]])
;</pre>

; <br> lastline 165 <br> 
 

; <pre>
        self.assertEqualMettaRunnerResults(metta1.run('!(how-it-works?)'), [[S('failure')]])
;</pre>

; <br>  lastline 170 <br> 
