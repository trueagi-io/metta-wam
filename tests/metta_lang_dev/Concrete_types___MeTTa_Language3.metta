
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(: a A)
(: foo (-&gt; A B))
! (get-type (foo a)) ; B

;  !(assertEqualToResult ( )  (  )  ) 
