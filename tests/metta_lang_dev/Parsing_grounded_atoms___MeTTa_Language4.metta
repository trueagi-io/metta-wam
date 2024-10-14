
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
from hyperon import *
metta = MeTTa()
# Getting a reference to a native GroundingSpace,
# implemented by the MeTTa core library.
grounding_space = GroundingSpaceRef()
grounding_space.add_atom(E(S(), S()))
space_atom = G(grounding_space)
# Registering a new custom token based on a regular expression.
# The new token can be used in a MeTTa program.
metta.register_atom(, space_atom)
print(metta.run())

;  !(assertEqualToResult ( )  (  )  ) 
