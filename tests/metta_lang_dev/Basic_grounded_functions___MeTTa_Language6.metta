
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;! (== 4 (+ 2 2)) ; True
;! (==) ; False
;! (== (A B) (A B)) ; True

;! (== 4 (+ 2 2)) 
; ready works
!(assertEqualToResult ( == 4 (+ 2 2) )  ( True )  ) 
; True

;! (== "This is a string" "Just a string") 
; ready works
!(assertEqualToResult ( == "This is a string" "Just a string" )  ( False )  ) 

; False
;! (== (A B) (A B)) 
; ready works
!(assertEqualToResult ( == (A B) (A B) )  ( True )  ) 
; True
;! (== (A B) (A (B C))) 

; ready works
!(assertEqualToResult ( == (A B) (A (B C)) )  ( False )  ) 
; False


;[True]
;[False]
;[True]
;[False]

;  !(assertEqualToResult ( )  (  )  ) 
