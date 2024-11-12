
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;! (let (test 1) 2 NO ) ; empty(= (is-frog Sam) True)
;(= (print-if-frog $x)
;   (let True (is-frog $x)
;     (println! ($x is frog!))))
;! (print-if-frog Sam) ; ()


(= (fact Sam) (age 34))
(= (fact Sam) (color green))
(= (fact Tom) (age 14))

;! (let (age $r) (fact $who)   ($who is $r)) 

; ready works
!(assertEqualToResult ( let (age $r) (fact $who)   ($who is $r) )  ( (Sam is 34) (Tom is 14)  )  ) 

; [(Tom is 14), (Sam is 34)]




;  !(assertEqualToResult ( )  (  )  ) 
