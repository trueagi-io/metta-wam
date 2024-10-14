
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (pragma! type-check auto) ; ()
(: foo (-&gt; Number Bool))
(= (foo $x) (+ $x 1)) ; BadType

;  !(assertEqualToResult ( )  (  )  ) 
