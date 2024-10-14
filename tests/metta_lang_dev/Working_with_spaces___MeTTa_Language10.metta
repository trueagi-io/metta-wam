
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(= (get-hello) &amp;hello)
! (bind! &amp;hello (Hello world)) ; ()
! (get-metatype &amp;hello) ; Expression
! &amp;hello ; (Hello world)

;  !(assertEqualToResult ( )  (  )  ) 
