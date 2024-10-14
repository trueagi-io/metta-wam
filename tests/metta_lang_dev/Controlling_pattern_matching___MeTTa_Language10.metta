
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (let $x (+ 1 2) (quote $x)) ; (quote 3)
(: Z Nat)
! (get-metatype (get-type Z)) ; (get-type Z) is Expression

;  !(assertEqualToResult ( )  (  )  ) 
