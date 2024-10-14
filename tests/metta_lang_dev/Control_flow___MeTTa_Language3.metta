
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (foo -9) ; 9! (if $x (+ 6 1) (- 7 2))
(= (foo $b $x)
   (if $b
       (+ $x 10)
       (* $x -1)
   )
)

;  !(assertEqualToResult ( )  (  )  ) 
