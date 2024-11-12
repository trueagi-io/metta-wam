
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 

; !(respond me)
(= (respond me) (ok I will respond))


;!(assertEqualToResult ( (respond me) )  ( (ok I will respond) (respond me) )  ) 

; notready gives error
!(assertEqualToResult ( (respond me) )  ( (ok I will respond) )  ) 
