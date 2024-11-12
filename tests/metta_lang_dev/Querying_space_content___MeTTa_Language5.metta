
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (match &amp;self (Parent Bob $x) $x) ; Ann(Parent Tom Bob)
(Parent Pam Bob)
(Parent Tom Liz)
(Parent Bob Ann)
(Parent Bob Pat)
(Parent Pat Pat)
! (match &amp;self (Parent $x $y) ($x $y))

;  !(assertEqualToResult ( )  (  )  ) 
