
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(Green Sam)
(Likes Sam Emi)
(Likes Tod Kat)
! (match &amp;self (Green $who)
    (match &amp;self (Likes $who $x) $x))
! (match &amp;self (Green $who)
    (match &amp;self (Likes $boo $x) $x))
! (match &amp;self (Likes $who $x)
    (match &amp;self (Green $x) $x))
! (match &amp;self (Likes $who $x)
    (match &amp;self (Green $boo) $boo))

;  !(assertEqualToResult ( )  (  )  ) 
