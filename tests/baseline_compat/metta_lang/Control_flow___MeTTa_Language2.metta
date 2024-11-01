
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(= (foo $x)
   (if (>= $x 0)
       (+ $x 10)
       (* $x -1)
   )
)

; notready
!(assertEqualToResult ( foo 1 )  ( 11 )  ) 


;! ()  ; 11

; ready works 
!(assertEqualToResult ( foo -9 )  ( 9 )  ) 


;! (foo -9) ; 9


;  !(assertEqualToResult ( )  (  )  ) 
