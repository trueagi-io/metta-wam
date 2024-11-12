
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
from hyperon import V
var_atom = V()
print(var_atom) # $x
print(var_atom.get_name()) # x
print(var_atom.get_metatype()) # AtomKind.VARIABLE
print(type(var_atom)) # VariableAtom

;  !(assertEqualToResult ( )  (  )  ) 
