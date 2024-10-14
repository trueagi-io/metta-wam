
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(= (get-space) (new-space))
! (add-atom (get-space) (Parent Bob Ann)) ; ()
! (match (get-space) (Parent $x $y) ($x $y)) ; nothing

;  !(assertEqualToResult ( )  (  )  ) 
