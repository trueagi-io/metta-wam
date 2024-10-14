
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(Parent Bob Ann)
! (assertEqual
    (match &amp;self (Parent Tom $x) $x)
    (empty)) ; ()
! (assertEqualToResult
    (match &amp;self (Parent Tom $x) $x)

;  !(assertEqualToResult ( )  (  )  ) 
