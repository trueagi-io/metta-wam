
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 94../../../hyperon-experimental/python/tests/test_atom_type.py :29
; <pre>
        self.assertEqual(get_atom_types(space, E(S("foo"), S("a"))), [S("B")])
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 29 <br> 
 

; <pre>
        self.assertEqual(get_atom_types(space, E(S("foo"), S("b"))),
                [])
;</pre>

; <br> lastline 31 <br> 
 

; <pre>
        self.assertEqual(get_atom_types(space, S("foo")), [E(S("->"), S("A"), S("B"))])
;</pre>

; <br>  lastline 30 <br> 
