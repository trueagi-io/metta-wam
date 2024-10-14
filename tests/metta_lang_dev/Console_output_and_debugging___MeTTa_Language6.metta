
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
   ())(Fruit apple)
(= (fruit $x)
   (match &amp;self (Fruit $x) $x))
! (fruit $x) ; apple

;  !(assertEqualToResult ( )  (  )  ) 
