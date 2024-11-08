
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
print(atom2) # (A B)from hyperon import MeTTa
metta = MeTTa()
program = metta.parse_all()
print(program) # [(A B), (C D)]

;  !(assertEqualToResult ( )  (  )  ) 
