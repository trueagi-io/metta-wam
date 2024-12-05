
; 20241014 r.v.v converted metta-lang.dev to metta 

(= (fact Sam) (age 34))
(= (fact Sam) (color green))
(= (fact Tom) (age 14))


;! (let (age $r) (fact $who)
;   ($who is $r)) 
; [(Tom is 14), (Sam is 34)]



!(println "xx 17 ") 
; ready works 
!(assertEqualToResult ( let (age $r) (fact $who) ($who is $r) ) ( (Tom is 14) (Sam is 34) ) ) 


