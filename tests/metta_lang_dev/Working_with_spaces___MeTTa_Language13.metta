
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! &amp;res! (bind! &amp;space (new-space)) ; ()
! (add-atom &amp;space (Parent Bob Ann)) ; ()
! &amp;space ; GroundingSpace-addr
! (match &amp;space (Parent $x $y) ($x $y)) ; (Bob Ann)
! (match &amp;self (Parent $x $y) ($x $y)) ; empty

;  !(assertEqualToResult ( )  (  )  ) 
