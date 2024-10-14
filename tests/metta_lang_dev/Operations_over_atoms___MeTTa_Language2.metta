
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (car-atom (seg (point 1 1) (point 1 4))) ; seg! (get-type cdr-atom) ; (-&gt; Expression %Undefined%)
! (cdr-atom (1 2 3)) ; (2 3)
! (cdr-atom (Cons X Nil)) ; (X Nil)
! (cdr-atom (seg (point 1 1) (point 1 4))) ; ((point 1 1) (point 1 4))

;  !(assertEqualToResult ( )  (  )  ) 
