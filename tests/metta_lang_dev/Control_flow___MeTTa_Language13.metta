
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (persons-of-age 34 14)(Sam is 34)
(= (age++ $who)
   (let* (($age (match &amp;self ($who is $a) $a))
          ( ()  (println! (WAS: ($who is $age))))
          ( ()  (remove-atom &amp;self ($who is $age)))
          ( ()  (add-reduct &amp;self ($who is (+ $age 1))))
          ($upd (match &amp;self ($who is $a) $a))
          ( ()  (println! (NOW: ($who is $upd)))))
        $upd
   )
)
! (age++ Sam) ; 35

;  !(assertEqualToResult ( )  (  )  ) 
