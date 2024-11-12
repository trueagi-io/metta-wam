
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(= (rps-winner $x $y)
   (case ($x $y)
     (((Paper Rock) First)
      ((Scissors Paper) First)
      ((Rock Scissors) First)
      (($a $a) Draw)
      ($_ Second))
   )
)
! (rps-winner Paper Scissors) ; Second
! (rps-winner Rock  Scissors) ; First
! (rps-winner Paper Paper   ) ; Draw

;  !(assertEqualToResult ( )  (  )  ) 
