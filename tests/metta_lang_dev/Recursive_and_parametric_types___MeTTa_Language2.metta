
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (get-type (S S)) ; not Nat(: Z Nat)
(: S (-&gt; Nat Nat))
(: Greater (-&gt; Nat Nat Bool))
(= (Greater (S $x) Z)
   True)
(= (Greater Z $x)
   False)
(= (Greater (S $x) (S $y))
   (Greater $x $y))
! (Greater (S Z) (S Z)) ; False
! (Greater (S (S Z)) (S Z)) ; True

;  !(assertEqualToResult ( )  (  )  ) 
