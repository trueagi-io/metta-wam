
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
))from hyperon import *
def print_all(*args):
    for a in args:
        print(a)
    return [Atoms.UNIT]
metta = MeTTa()
metta.register_atom(, OperationAtom(, print_all))
metta.run()

;  !(assertEqualToResult ( )  (  )  ) 
