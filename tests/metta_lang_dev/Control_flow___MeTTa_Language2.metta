
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(= (foo $x)
   (if (&gt;= $x 0)
       (+ $x 10)
       (* $x -1)
   )
)
! (foo 1)  ; 11

;  !(assertEqualToResult ( )  (  )  ) 
