
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(= (color) red)
(= (color) green)
(= (color) blue)
(= (fruit) apple)
(= (fruit) banana)
(= (fruit) mango)
(= (filter-prefer blue) blue)
(= (filter-prefer banana) banana)
(= (filter-prefer mango) mango)
(= (filter-prefer $x) (empty))
! (filter-prefer (color)) ; [blue]
! (filter-prefer (fruit)) ; [mango, banana]

;  !(assertEqualToResult ( )  (  )  ) 
