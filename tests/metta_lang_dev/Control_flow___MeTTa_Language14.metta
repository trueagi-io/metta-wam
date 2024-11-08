
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(Sam is Frog)
(Apple is Green)
(= (test $who)
   (case (match &amp;self ($who is $x) $x)
    (
        (42)
        (Frog )
        ($a   ($who is $a))
    )))
! (test Sam) ; "Do not ask me about frogs"
! (test Apple) ; (Apple is Green)
! (test Car) ; empty

;  !(assertEqualToResult ( )  (  )  ) 
