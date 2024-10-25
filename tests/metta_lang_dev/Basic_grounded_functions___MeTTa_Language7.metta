
; 20241014 r.v.v converted metta-lang.dev to metta 


;! (== 4 (+ Q 2)) ; False
;(: R CustomType)
;! (== 4 R) ; BadType

;  !(assertEqualToResult ( )  (  )  ) 
;! (== (A B) (A (B C))) 
; False

;!(== 4 (+ Q 2)) 

; ready works
!(assertEqualToResult ( == 4 (+ Q 2) )  ( False )  ) 

; False
(: R CustomType)
; !(== 4 R) 
; BadType
; ready works
!(assertEqualToResult ( == 4 R )  ( False )  ) 
