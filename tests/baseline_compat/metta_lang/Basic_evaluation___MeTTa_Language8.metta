
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(= (check ($x $y $x)) ($x $y))
;! (check (B A B)) ; reduced to (B A)


; ready works
!(assertEqualToResult ( (check (B A B)) )  ( ((B A)) )  ) 

; ready works
; notready not ready 
!(assertEqualToResult  (check (B A A)) ((check (B A A))))




