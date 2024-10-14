
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(+ 1 False)
! (match &amp;self (+ 1 False) OK) ; OK
! (match &amp;self (+ 1 $x) $x) ; False

;  !(assertEqualToResult ( )  (  )  ) 
