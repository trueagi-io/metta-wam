
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
from hyperon import MeTTa
metta = MeTTa()
print(metta.run()[0])    # [(A B)]
print(metta.run()[0])  # [3]
print(metta.parse_all())   # [(A B)]
print(metta.parse_all()) # [(+ 1 2)]

;  !(assertEqualToResult ( )  (  )  ) 
