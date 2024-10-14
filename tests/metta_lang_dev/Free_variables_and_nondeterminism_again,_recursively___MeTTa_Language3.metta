
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
; random bit
(= (bin) 0)
(= (bin) 1)
; binary list
(= (gen-bin $n)
   (if (&gt; $n 0)
       (:: (bin) (gen-bin (- $n 1)))
       ()))
! (gen-bin 3)

;  !(assertEqualToResult ( )  (  )  ) 
