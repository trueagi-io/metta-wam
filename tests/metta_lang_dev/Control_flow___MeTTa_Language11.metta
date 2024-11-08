
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
   (match &amp;self ($who is $r years old) $who)) ; Sam(= (fact Sam) (age 34))
(= (fact Sam) (color green))
(= (fact Tom) (age 14))
! (let (age $r) (fact $who)
   ($who is $r)) ; [(Tom is 14), (Sam is 34)]

;  !(assertEqualToResult ( )  (  )  ) 
