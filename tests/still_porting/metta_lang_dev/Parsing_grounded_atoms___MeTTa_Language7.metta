
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
from hyperon import *
def find_pos(x:str, y=, left=True):
    if left:
        return x.find(y)
    pos = x[-1:].find(y)
    return len(x) - 1 - pos if pos &gt;= 0 else pos
metta = MeTTa()
metta.register_atom(, OperationAtom(, find_pos))
print(metta.run(
))

;  !(assertEqualToResult ( )  (  )  ) 
