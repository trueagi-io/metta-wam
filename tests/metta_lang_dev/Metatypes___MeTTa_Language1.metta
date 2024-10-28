
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


;! (get-metatype 1) 
; Grounded
!(println "xx 25 ") 
 ; ready works 
!(assertEqualToResult ( get-metatype 1 ) ( Grounded ) ) 



;! (get-metatype +) 
; Grounded
!(println "xx 26 ") 
 ; ready works  
!(assertEqualToResult ( get-metatype + ) ( Grounded ) ) 


;! (get-metatype (+ 1 2)) 
; Expression
!(println "xx 27 ") 
 ; notready 
!(assertEqualToResult ( get-metatype (+ 1 2) ) ( Expression ) ) 


;! (get-metatype a) 
; Symbol
!(println "xx 28 ") 
 ; ready works  
!(assertEqualToResult ( get-metatype a ) ( Symbol ) ) 

;! (get-metatype (a b)) 
; Expression
!(println "xx 29 ") 
 ; ready works  
!(assertEqualToResult ( get-metatype (a b) ) ( Expression ) ) 


;! (get-metatype $x) 
; Variable
!(println "xx 30 ") 
 ; ready works  
!(assertEqualToResult ( get-metatype $x ) ( Variable ) ) 



;  !(assertEqualToResult ( )  (  )  ) 
