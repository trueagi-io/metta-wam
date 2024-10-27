
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(= (bin) 0)
(= (bin) 1)
(= (bin2) (superpose (0 1)))


;! (bin) 
; [0, 1]


!(println "xx 6 ") 
 ; ready works 
!(assertEqualToResult ( bin ) ( 0 1 ) ) 



;! (bin2) 
; [0, 1]


!(println "xx 7 ") 
 ; ready works  
!(assertEqualToResult ( bin2 ) ( 0 1 ) ) 




;  !(assertEqualToResult ( )  (  )  ) 
