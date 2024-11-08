
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(Parent Bob Ann)
; This match will be successful
! (match &amp;self (Parent Bob Ann) (Bob is Ann`s father))
; The following line will return []
! (match &amp;self (Parent Bob Joe) (Bob is Joe`s father))

;  !(assertEqualToResult ( )  (  )  ) 
