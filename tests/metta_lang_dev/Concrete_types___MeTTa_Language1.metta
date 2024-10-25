
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(: a A)
(: b B)
(: A Type)
;! (get-type a) 
; ready works
!(assertEqualToResult ( get-type a )  ( A )  ) 
; A
;! (get-type b) 
; ready works
!(assertEqualToResult ( get-type b )  (  B )  ) 
; B
;! (get-type c) 
; ready works
!(assertEqualToResult ( get-type c )  ( %Undefined% )  ) 
; 
;! (get-type A) 
; ready works
!(assertEqualToResult ( get-type A )  ( Type )  ) 
; Type

;  !(assertEqualToResult ( )  (  )  ) 

