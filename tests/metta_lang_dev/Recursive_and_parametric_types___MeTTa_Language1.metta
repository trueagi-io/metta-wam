
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(: Z Nat) ; Z is "zero"
(: S (-&gt; Nat Nat)) ; S "constructs" the next number
! (S Z) ; this is "one"
! (S (S Z)) ; this is "two"
! (get-type (S (S (S Z)))) ; Nat

;  !(assertEqualToResult ( )  (  )  ) 
