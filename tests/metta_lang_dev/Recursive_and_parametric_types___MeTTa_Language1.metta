
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(: Z Nat) 
; Z is "zero"
(: S (-> Nat Nat)) 
; S "constructs" the next number

;! (S Z) 
; this is "one"
!(println "xx 73 ") 
 ; notready 
!(assertEqualToResult ( S Z ) ( this is "one" ) ) 


;! (S (S Z)) 
; this is "two"
!(println "xx 74 ") 
 ;  notready 
!(assertEqualToResult ( S (S Z) ) ( this is "two" ) ) 


;! (get-type (S (S (S Z)))) 
; Nat

!(println "xx 75 ") 
 ; ready works 
!(assertEqualToResult ( get-type (S (S (S Z))) ) ( Nat ) ) 

;! (get-type (S S)) 
; not Nat

!(println "xx 76 ") 
 ; notready 
!(assertEqualToResult ( get-type (S S) ) ( not Nat ) ) 



;  !(assertEqualToResult ( )  (  )  ) 
