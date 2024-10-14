
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(Parent Tom Bob)
(Parent Bob Ann)
(Implies (Parent $x $y) (Child $y $x))
(= (deduce $B)
   (match &amp;self (Implies $A $B)
          (match &amp;self $A $B))
)
(= (conclude $A)
   (match &amp;self (Implies $A $B)
          (match &amp;self $A $B))
)
! (deduce (Child $x Tom))    ; [(Child Bob Tom)]
! (conclude (Parent Bob $y)) ; [(Child Ann Bob)]

;  !(assertEqualToResult ( )  (  )  ) 
