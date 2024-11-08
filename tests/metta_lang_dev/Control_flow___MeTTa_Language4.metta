
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
!;  ((foo $b 1) $b) 


; ready works 
!(assertEqualToResult ( if (> $x 0) (+ $x 5) (- $x 5) )  (  (- $x 5)  )  ) 
;! (if (> $x 0) (+ $x 5) (- $x 5))



; [(-1 False), (11 True)]! (if (&gt; $x 0) (+ $x 5) (- $x 5))

;  !(assertEqualToResult ( )  (  )  ) 
