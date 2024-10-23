
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 

(= (duplicate $x) ($x $x))
; ! (duplicate A)
; ready works
!(assertEqualToResult ( duplicate A )  ( (A A) )  ) 


; ! (duplicate A)
; ready works
!(assertEqualToResult ( duplicate 1.05 )  ( (1.05 1.05) )  ) 


; ! (duplicate 1.05)


;[(A A)]
;[(1.05 1.05)]

; ready works 
(= (swap (Pair $x $y)) (Pair $y $x))
!(assertEqualToResult ( swap (Pair A B) )  ( (Pair B A) )  ) 

;! (swap (Pair A B)) ; 

; evaluates to (Pair B A)

;[(Pair B A)]





;  !(assertEqualToResult ( )  (  )  ) 
