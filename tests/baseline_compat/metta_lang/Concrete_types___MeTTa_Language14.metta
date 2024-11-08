
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;! (get-type (+ 1 2.1)) 
; Number(: a A)
;! (get-type (+ 1 a)) 
; empty
;! (get-type (+ 1 b)) 
; Number
;! (+ 1 b) 
; no error, not reduced
;! (+ 1 a) 
; type error


;! (get-type 1) 
; Number
!(assertEqualToResult ( get-type 1 )  ( Number )  ) 

;! (get-type 1.1) 
; Number
;!(assertEqualToResult ( get-type 1.1 )  ( Number )  ) 

;! (get-type +) 
; (-> Number Number Number)
;!(assertEqualToResult ( get-type + )  (  (-> Number Number Number) )  ) 

;! (get-type (+ 1 2.1)) 
; Number
;!(assertEqualToResult ( get-type (+ 1 2.1) )  ( Number )  ) 


;  !(assertEqualToResult ( )  (  )  ) 
