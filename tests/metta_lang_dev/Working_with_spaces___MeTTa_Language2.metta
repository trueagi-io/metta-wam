
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
    (quote $x)) ; [(quote (+ 1 2)), (quote (+ 3 4))]! (add-reduct &amp;self (= (foo) (+ 3 4))) ; ()
! (foo) ; 7
! (match &amp;self (= (foo) $x)
    (quote $x)) ; (quote 7)

;  !(assertEqualToResult ( )  (  )  ) 
