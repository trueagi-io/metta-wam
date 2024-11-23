
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
def plus(atom1, atom2):
    from hyperon import ValueAtom
    sum = atom1.get_object().value + atom2.get_object().value
    return [ValueAtom(sum, )]
from hyperon import OperationAtom, MeTTa
plus_atom = OperationAtom(, plus,
    [, , ], unwrap=False)
metta = MeTTa()
metta.register_atom(, plus_atom)
print(metta.run())

;  !(assertEqualToResult ( )  (  )  ) 
