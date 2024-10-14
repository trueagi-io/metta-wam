
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(= (apply-twice $f $x)
   ($f ($f $x)))
(= (square $x) (* $x $x))
(= (duplicate $x) ($x $x))
! (apply-twice square 2) ; 16
! (apply-twice duplicate 2) ; ((2 2) (2 2))
! (apply-twice 1 2) ; (1 (1 2))

;  !(assertEqualToResult ( )  (  )  ) 
