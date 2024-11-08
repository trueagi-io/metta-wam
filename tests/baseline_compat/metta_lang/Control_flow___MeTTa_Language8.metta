
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;(= (test 1) 2)
;! (let 2 (test 1) YES) ; YES


(Sam is 34 years old)

;! (match &self ($who is (+ 20 14) years old) $who) 
; ready works
!(assertEqualToResult ( match &self ($who is (+ 20 14) years old) $who )  (  )  ) 


; empty
;! (let $r (+ 20 14)
;   (match &self ($who is $r years old) $who)) 

; ready works
!(assertEqualToResult ( let $r (+ 20 14) (match &self ($who is $r years old) $who) )  (  Sam )  ) 


; Sam

;  !(assertEqualToResult ( )  (  )  ) 
