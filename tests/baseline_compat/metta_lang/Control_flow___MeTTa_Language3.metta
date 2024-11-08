
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;! (if $x (+ 6 1) (- 7 2))

; ready works 
!(assertEqualToResult ( if $x (+ 6 1) (- 7 2) )  ( 5 )  ) 


(= (foo $b $x)
   (if $b
       (+ $x 10)
       (* $x -1)
   )
)


; ! ((foo $b 1) $b) 
; ready works 
!(assertEqualToResult ( (foo $b 1) $b  )  ( (-1 $b ) )  ) 


; [(-1 False), (11 True)]



;  !(assertEqualToResult ( )  (  )  ) 
