
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 



(= (color) green)
(= (color) yellow)
(= (color) red)

;! (color) 
; [green, yellow, red]


!(println "xx 19 ") 
; ready works 
!(assertEqualToResult ( color ) ( green yellow red ) ) 



;! (collapse (color)) 
; [(green yellow red)]


!(println "xx 20 ") 
; ready works  
!(assertEqualToResult ( collapse (color) ) ( (green yellow red) ) ) 


;! (let $x (collapse (color))
;    (superpose $x))  
; [green, yellow, red]


!(println "xx 21 ") 
; ready works  
!(assertEqualToResult ( let $x (collapse (color)) (superpose $x) ) ( green yellow red ) ) 


;! (superpose (1 2 3)) 
; [1, 2, 3]


!(println "xx 22 ") 
; ready works  
!(assertEqualToResult ( superpose (1 2 3) ) ( 1 2 3 ) ) 

;! (collapse (superpose (1 2 3)))
; [(1 2 3)]


!(println "xx 23 ") 
; ready works  
!(assertEqualToResult ( collapse (superpose (1 2 3)) ) ( (1 2 3) ) ) 



;! (let $x (superpose (1 2 3)) 
;    (collapse $x))    
; [(1), (2), (3)]



!(println "xx 24 ") 
; ready works  
!(assertEqualToResult ( let $x (superpose (1 2 3)) (collapse $x) ) ( (1) (2) (3) ) ) 





;  !(assertEqualToResult ( )  (  )  ) 
