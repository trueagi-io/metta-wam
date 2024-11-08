
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 

(+ 1 False)
;! () ; OK
; ready works
!(assertEqualToResult ( match &self (+ 1 False) OK )  ( OK )  ) 


; ready works
;! () ; False

; ready works
!(assertEqualToResult ( match &self (+ 1 $x) $x )  ( False )  ) 



;  !(assertEqualToResult ( )  (  )  ) 
