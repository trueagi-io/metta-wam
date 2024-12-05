
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;! (+ 1 (+ 2 (+ 3 4))) ; 
; ready works
!(assertEqualToResult ( + 1 (+ 2 (+ 3 4)) )  ( 10 )  ) 
;! (- 8 (/ 6.4 4)) ; 
; ready works
!(assertEqualToResult ( - 8 (/ 6.4 4) )  ( 6.4 )  ) 

;  !(assertEqualToResult ( )  (  )  ) 
