
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(: foo (-&gt; A B))
(: bar (-&gt; B B A))
(: a A)
! (get-type (bar (foo a) (foo a)))

;  !(assertEqualToResult ( )  (  )  ) 
