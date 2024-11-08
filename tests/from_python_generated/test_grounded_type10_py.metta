
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 10../../../hyperon-experimental/python/tests/test_grounded_type.py :24
; <pre>
        self.assertEqual(
            metta.run("!(untop)")[0][0],
            metta.parse_single("()"))
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 26 <br> 
 

; <pre>
            metta.run("!(> 1 1)")[0][0].get_grounded_type(),
;</pre>

; <br> lastline 28 <br> 
 

; <pre>
            metta.run("!(+ 1 1)")[0][0].get_grounded_type())

    def test_higher_func(self):
        metta = MeTTa(env_builder=Environment.test_env())
        metta.register_atom(
;</pre>

; <br>  lastline 42 <br> 
