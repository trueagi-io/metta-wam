
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
from hyperon import *
metta = MeTTa()
plus = metta.parse_single()
print(type(plus.get_object())) # OperationObject
print(plus.get_object().op) # some lambda
print(plus.get_object()) # + as a representation of this operation
calc = metta.run()[0][0]
print(type(calc.get_object())) # ValueObject
print(calc.get_object().value) # 3
metta.run() # add the expression to the space
pattern = E(V(), ValueAtom(42))
print(metta.space().query(pattern)) # { $x &lt;- my-secret-symbol }

;  !(assertEqualToResult ( )  (  )  ) 
