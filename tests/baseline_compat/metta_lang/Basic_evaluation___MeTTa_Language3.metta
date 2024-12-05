
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(= (only-a A) (Input A is accepted))
;! (only-a A)
;! (only-a B)

;[(Input A is accepted)]
;[(only-a B)]

; ready works
!(assertEqualToResult ( only-a A )  ( (Input A is accepted ) ) ) 

; ready works
!(assertEqualToResult ( only-a B )  ( (only-a B ) ) ) 
