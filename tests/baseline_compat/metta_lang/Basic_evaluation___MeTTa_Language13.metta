
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(= (Car (Cons $x $xs)) $x)
(= (Second (Cons $x $xs)) (Car $xs))
; outputs B

; ready works 
!(assertEqualToResult ( (Second (Cons A (Cons B Nil))) )  ( B )  ) 
