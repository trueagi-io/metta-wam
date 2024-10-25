
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 

;! (+ 2 S) 
; ready works
!(assertEqualToResult ( + 2 S )  ( (+ 2 S) )  ) 
; (+ 2 S)
;! (+ 2 "8") 
; BadType
; notready 
!(assertEqualToResult ( + 2 "8" )  ( BadType )  ) 
