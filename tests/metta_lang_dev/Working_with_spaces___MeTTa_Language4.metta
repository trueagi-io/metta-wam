
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (remove-atom &amp;self (Atom to remove)) ; ()(link A B)
(link B C)
(link C A)
(link C E)
! (match &amp;self (, (link $x $y)
                  (link $y $z)
                  (link $z $x))
               (let () (remove-atom &amp;self (link $x $y))
                       (add-atom &amp;self (link $y $x)))
  ) ; [(), (), ()]
! (match &amp;self (link $x $y)
    (link $x $y)) ; [(link A C), (link C B), (link B A), (link C E)]

;  !(assertEqualToResult ( )  (  )  ) 
