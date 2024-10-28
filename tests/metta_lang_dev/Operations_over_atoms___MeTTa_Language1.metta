
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


;! (get-type car-atom) 
; (-> Expression %Undefined%)


!(println "xx 44 ") 
 ;  notready 
!(assertEqualToResult ( get-type car-atom ) ( (-> Expression %Undefined%) ) ) 


;! (car-atom (1 2 3)) 
; 1

!(println "xx 45 ") 
 ; ready works 
!(assertEqualToResult ( car-atom (1 2 3) ) ( 1 ) ) 


;! (car-atom (Cons X Nil)) 
; Cons

!(println "xx 46 ") 
 ; ready works 
!(assertEqualToResult ( car-atom (Cons X Nil) ) ( Cons ) ) 



;! (car-atom (seg (point 1 1) (point 1 4))) 
; seg
!(println "xx 47 ") 
 ; ready works 
!(assertEqualToResult ( car-atom (seg (point 1 1) (point 1 4)) ) ( seg ) ) 




;  !(assertEqualToResult ( )  (  )  ) 
