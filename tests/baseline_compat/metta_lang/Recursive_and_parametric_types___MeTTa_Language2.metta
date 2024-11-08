
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(: Z Nat)
(: S (-> Nat Nat))
(: Greater (-> Nat Nat Bool))
(= (Greater (S $x) Z)
   True)
(= (Greater Z $x)
   False)
(= (Greater (S $x) (S $y))
   (Greater $x $y))


;! (Greater (S Z) (S Z)) 
; False

!(println "xx 77 ") 
 ; ready works 
!(assertEqualToResult ( Greater (S Z) (S Z) ) ( False ) ) 



;! (Greater (S (S Z)) (S Z)) 
; True

!(println "xx 78 ") 
 ; ready works 
!(assertEqualToResult ( Greater (S (S Z)) (S Z) ) ( True ) ) 


;  !(assertEqualToResult ( )  (  )  ) 
