
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 14../../../hyperon-experimental/python/tests/test_grounded_type.py :92
; <pre>
        self.assertEqual(metta.run('''
            !(id_undef 1)
            !(id_undef False)
            !(id_undef (+ 1 1))
            ''', flat=True),
            metta.parse_all("1 False 2"))
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 97 <br> 
 

; <pre>

            !(id_undef myAtom)
            !(id_undef untyp)
            ''', flat=True),
            metta.parse_all('''
             (id_undef myAtom)
             (id_undef untyp)
            
;</pre>

; <br>  lastline 11 <br> 
