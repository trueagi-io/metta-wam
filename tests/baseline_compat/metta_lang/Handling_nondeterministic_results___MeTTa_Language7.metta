
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 

(= (color) red)
(= (color) green)
(= (color) blue)

;! (color) 
; three results: [blue, red, green]


!(println "xx 17 ") 
; ready works 
!(assertEqualToResult ( color ) ( blue red green ) ) 



;! (collapse (color)) 
; one result: [(blue red green)]


!(println "xx 18 ") 
; ready works 
!(assertEqualToResult ( collapse (color) ) ( (blue red green) ) ) 


;  !(assertEqualToResult ( )  (  )  ) 
