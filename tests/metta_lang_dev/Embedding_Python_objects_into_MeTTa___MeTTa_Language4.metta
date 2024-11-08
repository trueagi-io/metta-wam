
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (np-arange (Kwargs (start 2) (stop 10) (step 3))) ; array([2, 5, 8])! (bind! np-rnd (py-atom numpy.random))
! ((py-dot np-rnd randint) 25)

;  !(assertEqualToResult ( )  (  )  ) 
