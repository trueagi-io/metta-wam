
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
from hyperon import *
metta = MeTTa()
expr1 = metta.parse_single()
expr2 = E(S(), S(), S())
print(, expr1)
print(, expr2)
print(, expr1 == expr2)
for atom in expr1.get_children():
    print(f{atom}{type(atom)})

;  !(assertEqualToResult ( )  (  )  ) 
