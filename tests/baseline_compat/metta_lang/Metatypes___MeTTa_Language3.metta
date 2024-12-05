
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


;! (get-type quote)
;[(-> Atom Atom)]
!(println "xx 35 ") 
 ; ready works  
!(assertEqualToResult ( get-type quote) (-> Atom Atom ) ( ) ) 



;! (quote (+ 1 2))
;[(quote (+ 1 2))]
!(println "xx 36 ") 
 ; ready works  
!(assertEqualToResult ( quote (+ 1 2)) (quote (+ 1 2) ) ( ) ) 



;! (get-type if)
;[(-> Bool Atom Atom $t#223)]
!(println "xx 37 ") 
 ; ready works  
!(assertEqualToResult ( get-type if) (-> Bool Atom Atom $t#223 ) ( ) ) 



;  !(assertEqualToResult ( )  (  )  ) 
