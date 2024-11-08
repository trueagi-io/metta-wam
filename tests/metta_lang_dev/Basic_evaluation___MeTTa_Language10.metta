
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(= (f special-value) catched)
(= (f $x) $x)

; ! (f A) 
; ready works
!(assertEqualToResult ( f A )  ( (A) )  ) 

; A
; ! (f special-value) 
; both catched and special-value

; ready works
!(assertEqualToResult ( f special-value )  ( catched special-value )  ) 
