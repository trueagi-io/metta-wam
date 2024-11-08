
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 88../../../hyperon-experimental/python/tests/test_extend.py :92
; <pre>
        result = metta.run("!(import! &self error_pyext)")
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 92 <br> 
 

; <pre>
        self.assertEqual(S('Error'), result[0][0].get_children()[0])
;</pre>

; <br>  lastline 20 <br> 
