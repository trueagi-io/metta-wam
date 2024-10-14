
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(Parent Tom Bob)
(Parent Pam Bob)
(Parent Tom Liz)
(Parent Bob Ann)
(= (get-parent-entries $x $y)
   (match &amp;self (Parent $x $y) (Parent $x $y)))
(= (get-parents $x)
   (match &amp;self (Parent $y $x) $y))
! (get-parent-entries Tom $_)

;  !(assertEqualToResult ( )  (  )  ) 
