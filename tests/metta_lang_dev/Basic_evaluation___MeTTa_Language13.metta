
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (square 3)(= (Car (Cons $x $xs)) $x)
(= (Second (Cons $x $xs)) (Car $xs))
! (Second (Cons A (Cons B Nil))) ; outputs B

;  !(assertEqualToResult ( )  (  )  ) 
