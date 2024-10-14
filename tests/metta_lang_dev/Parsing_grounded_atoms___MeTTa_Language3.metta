
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
from hyperon import *
metta = MeTTa()
expr1 = metta.parse_single()
print(metta.evaluate_atom(expr1))
expr2 = E(OperationAtom(, lambda a, b: a + b),
          ValueAtom(1), ValueAtom(2))
print(metta.evaluate_atom(expr2))

;  !(assertEqualToResult ( )  (  )  ) 
