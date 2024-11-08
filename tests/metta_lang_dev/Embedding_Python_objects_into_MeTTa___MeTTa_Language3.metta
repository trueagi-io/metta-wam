
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (bind! np-arange (py-atom numpy.arange)) ; ()
! (np-arange 4) ; array([0, 1, 2, 3])
! (np-arange (Kwargs (step 2) (stop 8))) ; array([0, 2, 4, 6])

;  !(assertEqualToResult ( )  (  )  ) 
