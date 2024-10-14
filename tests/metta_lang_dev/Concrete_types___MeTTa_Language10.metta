
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (get-type (foo (bar (foo a) (foo a))))(: foo-pair (-&gt; (A B) C))
(: a A)
(: b B)
! (get-type (foo-pair a b)) ; empty
! (get-type (foo-pair (a b))) ; C

;  !(assertEqualToResult ( )  (  )  ) 
