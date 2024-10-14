
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(Parent Bob Ann)
! (assertEqual
   (match &amp;self (Parent $x Ann) $x)
   (unify (Parent $x Ann) (Parent Bob $y) $x Failed)) ; ()
! (assertEqual (+ 1 2) 3) ; ()
! (assertEqual (+ 1 2) (+ 1 4)) ; Error-expression

;  !(assertEqualToResult ( )  (  )  ) 
