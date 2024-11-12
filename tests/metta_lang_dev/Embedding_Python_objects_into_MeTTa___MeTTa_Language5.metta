
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (bind! np (py-atom numpy))
! ((py-dot np abs) -5)
! ((py-dot np random.randint) -25 0)

;  !(assertEqualToResult ( )  (  )  ) 
