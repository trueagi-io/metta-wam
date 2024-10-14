
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(= (zip () ()) ())
(= (zip (:: $x $xs) (:: $y $ys))
   (:: ($x $y) (zip $xs $ys)))
! (zip (:: A (:: B ())) (:: 1 (:: 2 ()))) ; (:: (A 1) (:: (B 2) ()))

;  !(assertEqualToResult ( )  (  )  ) 
