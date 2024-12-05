
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


; ready works
!(assertEqualToResult ( * (+ 1 2) (- 8 3) )  ( 15 )  ) 

(= (square $x) (* $x $x))

; ready works
!(assertEqualToResult ( square (+ 2 3) )  ( 25 )  ) 


(= (triple $x) ($x $x $x))
(= (grid3x3 $x) (triple (triple $x)))


;! (grid3x3 (square (+ 1 2))) ; ((9 9 9) (9 9 9) (9 9 9))

; ready works
!(assertEqualToResult ( grid3x3 (square (+ 1 2)) )  ( ((9 9 9) (9 9 9) (9 9 9)) )  ) 

;  !(assertEqualToResult ( )  (  )  ) 
