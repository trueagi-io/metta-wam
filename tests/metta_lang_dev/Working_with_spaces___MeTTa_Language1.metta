
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(: add-foo-eq (-&gt; Atom (-&gt;)))
(= (add-foo-eq $x)
   (add-atom &amp;self (= (foo) $x)))
! (foo) ; (foo) - not reduced
! (add-foo-eq (+ 1 2)) ; () - OK
! (add-foo-eq (+ 3 4)) ; () - OK
! (foo) ; [3, 7]
! (match &amp;self (= (foo) $x)

;  !(assertEqualToResult ( )  (  )  ) 
