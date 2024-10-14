
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (get-parents Bob)(Parent Tom Bob)
(Parent Pam Bob)
(Parent Tom Liz)
(Parent Bob Ann)
(Parent Bob Pat)
(Parent Pat Jim)
(= (get-parents $x)
   (match &amp;self (Parent $y $x) $y))
(= (get-grand-parents $x)
   (...))
! (get-grand-parents Pat)

;  !(assertEqualToResult ( )  (  )  ) 
