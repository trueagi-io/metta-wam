
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (foo b) ; type error(: foo (-&gt; A B))
(: a A)
! (get-type (foo c))
! (get-type (g a))

;  !(assertEqualToResult ( )  (  )  ) 
