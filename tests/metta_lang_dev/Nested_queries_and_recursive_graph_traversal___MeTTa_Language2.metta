
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(Female Pam)
(Male Tom)
(Male Bob)
(Female Liz)
(Female Pat)
(Female Ann)
(Male Jim)
(Parent Tom Bob)
(Parent Pam Bob)
(Parent Tom Liz)
(Parent Bob Ann)
(Parent Bob Pat)
(Parent Pat Jim)
(= (get-sister $x)
   (match &amp;self
     (, (Parent $y $x)
        (Parent $y $z)
        (Female $z))
     $z
   )
)
! (get-sister Bob)

;  !(assertEqualToResult ( )  (  )  ) 
