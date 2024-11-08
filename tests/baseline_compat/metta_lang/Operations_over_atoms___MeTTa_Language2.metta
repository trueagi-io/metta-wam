
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


;! (get-type cdr-atom) 
; (-> Expression %Undefined%)

!(println "xx 48 ") 
 ;  notready 
!(assertEqualToResult ( get-type cdr-atom ) ( (-> Expression %Undefined%) ) ) 



;! (cdr-atom (1 2 3)) 
; (2 3)

!(println "xx 49 ") 
 ; ready works 
!(assertEqualToResult ( cdr-atom (1 2 3) ) ( (2 3) ) ) 

;! (cdr-atom (Cons X Nil)) 
; (X Nil)

!(println "xx 50 ") 
 ; ready works  
!(assertEqualToResult ( cdr-atom (Cons X Nil) ) ( (X Nil) ) ) 

;! (cdr-atom (seg (point 1 1) (point 1 4))) 
; ((point 1 1) (point 1 4))

!(println "xx 51 ") 
 ; ready works  
!(assertEqualToResult ( cdr-atom (seg (point 1 1) (point 1 4)) ) ( ((point 1 1) (point 1 4)) ) ) 



;  !(assertEqualToResult ( )  (  )  ) 
