
; 20241014 r.v.v converted metta-lang.dev to metta 


(Sam is 34 years old)

;! (match &self ($who is (+ 20 14) years old) $who) 
; empty


!(println "xx 15 ") 
; ready works 
!(assertEqualToResult ( match &self ($who is (+ 20 14) years old) $who ) ( ) ) 


;! (let $r (+ 20 14)
;   (match &self ($who is $r years old) $who)) 
; Sam


!(println "xx 16 ") 
; ready works 
!(assertEqualToResult ( let $r (+ 20 14) (match &self ($who is $r years old) $who) ) ( Sam ) ) 

