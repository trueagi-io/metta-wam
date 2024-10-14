
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (collapse (color)) ; one result: [(blue red green)](= (color) green)
(= (color) yellow)
(= (color) red)
! (color) ; [green, yellow, red]
! (collapse (color)) ; [(green yellow red)]
! (let $x (collapse (color))
    (superpose $x))  ; [green, yellow, red]
! (superpose (1 2 3)) ; [1, 2, 3]
! (collapse (superpose (1 2 3)))
! (let $x (superpose (1 2 3)) ; [(1 2 3)]
    (collapse $x))    ; [(1), (2), (3)]

;  !(assertEqualToResult ( )  (  )  ) 
