
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 

(= (only-a A) (Input A is accepted))

;! (only-a A)
;[(Input A is accepted)]

!(println "xx1") 
 ; ready works  
!(assertEqualToResult ( only-a A ) ( (Input A is accepted) ) ) 


;! (only-a B)
;[(only-a B)]
!(println "xx2") 
 ; ready works  
!(assertEqualToResult ( only-a B ) ( (only-a B) ) ) 


;! (only-a $x3)
;[(Input A is accepted)]
!(println "xx3") 
 ; ready works 
!(assertEqualToResult ( only-a $x ) ( (Input A is accepted) ) ) 






;  !(assertEqualToResult ( )  (  )  ) 
