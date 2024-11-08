
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
from hyperon import *
metta = MeTTa()
entry = E(S(), G({: 1, : 2}))
metta.space().add_atom(entry)
result = metta.run()[0][0]
print(type(result)) # GroundedAtom
print(result.get_object()) # {'a': 1, 'b': 2}

;  !(assertEqualToResult ( )  (  )  ) 
