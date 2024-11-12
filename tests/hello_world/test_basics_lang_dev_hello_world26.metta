

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
! (if (frog $x) ($x is Frog) ($x is-not Frog))
; (green $x) is true if (frog $x) is true,
; otherwise it is not calculated.
(= (green $x)
   (if (frog $x) True (empty)))
; ! (if (green $x) ($x is Green) ($x is-not Green))


(= (green $x) (frog $x))


;This will result in (Sam is-not Green) to appear, which shows that (= (green $x)
; (frog $x)) is not the same as logical implication even with boolean 
;return values, although it is not precisely the same as 
;equivalence (more on this in another tutorial).

; fault 
;[(Fritz is Frog)]
;% 28,633 inferences, 0.026 CPU in 0.027 seconds (97% CPU, 1096315 Lips)