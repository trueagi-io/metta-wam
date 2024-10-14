
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (unify (parent $x Bob) ; the first pattern
         (parent Tom $y) ; the second pattern
         ($x $y) ; the output for successful unification

;  !(assertEqualToResult ( )  (  )  ) 
