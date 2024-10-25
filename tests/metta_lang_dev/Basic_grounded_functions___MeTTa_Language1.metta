
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
; Addition
;! (+ 1 3) 

; ready works
!(assertEqualToResult ( + 1 3 )  ( 4 )  ) 

; Subtraction
;! (- 6 2.2) ; 
; ready works
!(assertEqualToResult ( - 6 2.2 )  ( 3.8 )  ) 
; Multiplication
;! (* 7.3 9) ; 65.7
; ready works
!(assertEqualToResult ( * 7.3 9 )  ( 65.7 )  ) 
; Division
;! (/ 25 5) ; 5 or 5.0
; ready works
!(assertEqualToResult ( / 25 5 )  ( 5.0 )  ) 
; Modulus
;! (% 24 5) ; 4
; ready works
!(assertEqualToResult ( % 24 5 )  ( 4 )  ) 

;  !(assertEqualToResult ( )  (  )  ) 
