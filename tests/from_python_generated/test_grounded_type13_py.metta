
; 20241007 R.v.Vessum converted python source to metta 
; FILE: 13../../../hyperon-experimental/python/tests/test_grounded_type.py :64
; <pre>

            !(id_atom 1)
            !(id_atom myAtom)
            !(id_atom untyp)
            ''', flat=True),
            metta.parse_all('''
            1
            myAtom
            untyp
            
;</pre>
  !(assertEqualToResult ( )  (  )  )
; <br> lastline 73 <br> 
 

; <pre>

            !(id_poly_w 1)
            !(id_poly_w myAtom)
            !(id_poly_w untyp)
            !(id_poly_w (+ 1 1))
            !(+ 1 (id_poly_w 2))
            ''', flat=True),
            metta.parse_all('''
             1
             myAtom
             untyp
             2
             3
            
;</pre>

; <br>  lastline 92 <br> 
