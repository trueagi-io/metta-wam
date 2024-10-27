
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
; Some facts as very basic equalities


; Some facts as very basic equalities
(= (croaks Fritz) True)
(= (eats_flies Fritz) True)
(= (croaks Sam) True)
(= (eats_flies Sam) False)
; If something croaks and eats flies, it is a frog.
; Note that if either (croaks $x) or (eats_flies $x)
; is false, (frog $x) is also false.
(= (frog $x)
   (and (croaks $x)
        (eats_flies $x)))
;! (if (frog $x) ($x is Frog) ($x is-not Frog))
;[(Fritz is Frog), (Sam is-not Frog)]

!(println "xx1") 
 ; ready works  
!(assertEqualToResult ( if (frog $x) ($x is Frog) ($x is-not Frog) ) (  (Fritz is Frog) (Sam is-not Frog)  ) ) 

; (green $x) is true if (frog $x) is true,
; otherwise it is not calculated.
(= (green $x)
   (if (frog $x) True (empty)))

;! (if (green $x) ($x is Green) ($x is-not Green))
;[(Fritz is Green)]

!(println "xx2") 
 ; ready works 
!(assertEqualToResult ( if (green $x) ($x is Green) ($x is-not Green) )  ( (Fritz is Green) )  ) 



;  !(assertEqualToResult ( )  (  )  ) 
