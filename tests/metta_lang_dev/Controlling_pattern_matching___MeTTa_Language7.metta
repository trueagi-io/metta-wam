
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(: foo (-&gt; Number Bool))
(= (foo $x) (+ $x 1))
! (get-type (foo $x)) ; Bool
! (get-type (+ $x 1)) ; Number
! (get-type =) ; (-&gt; $t $t Atom)
! (= (foo $x) (+ $x 1)) ; BadType

;  !(assertEqualToResult ( )  (  )  ) 
