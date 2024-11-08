
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
          ($x is Bob`s child))) ; return the result of this expression(Parent Bob Ann)
! (trace! ((Expected: (Bob is Ann`s parent))
           (Got: (match &amp;self (Parent $x Ann) ($x is Ann`s parent)))
          )

;  !(assertEqualToResult ( )  (  )  ) 
