
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
! (if (green $x) ($x is Green) ($x is-not Green))


;[(Fritz is Frog), (Sam is-not Frog)]
;[(Fritz is Green)]

; fault

;[(Fritz is Frog)]
;[(Fritz is Green) ]
;% 41,351 inferences, 0.066 CPU in 0.068 seconds (97% CPU, 621972 Lips)