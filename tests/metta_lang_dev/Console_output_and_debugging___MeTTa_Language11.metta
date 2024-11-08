
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(Parent Bob Ann)
(Parent Pam Ann)
! (assertEqualToResult
    (match &amp;self (Parent $x Ann) $x)
    (Bob Pam))  ; ()
(= (bin) 0)
(= (bin) 1)
! (assertEqualToResult (bin) (0 1)) ; ()
! (assertEqualToResult (+ 1 2) (3)) ; ()
! (assertEqualToResult
    (+ 1 untyped-symbol)
   ((+ 1 untyped-symbol))) ; ()
! (assertEqualToResult (+ 1 2) ((+ 1 2))) ; Error

;  !(assertEqualToResult ( )  (  )  ) 
