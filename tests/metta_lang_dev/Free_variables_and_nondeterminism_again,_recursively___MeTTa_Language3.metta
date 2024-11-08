
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
; random bit

; random bit
(= (bin) 0)
(= (bin) 1)
; binary list
(= (gen-bin $n)
   (if (> $n 0)
       (:: (bin) (gen-bin (- $n 1)))
       ()))
;! (gen-bin 3)
; (:: 1 (:: 0 (:: 1 ()))), (:: 1 (:: 0 (:: 0 ()))), (:: 1 (:: 1 (:: 1 ()))), (:: 1 (:: 1 (:: 0 ()))), (:: 0 (:: 0 (:: 1 ()))), (:: 0 (:: 0 (:: 0 ()))), (:: 0 (:: 1 (:: 1 ()))), (:: 0 (:: 1 (:: 0 ())))]

!(println "xx") 
; ready works 
!(assertEqualToResult ( gen-bin 3 ) (  (:: 1 (:: 0 (:: 1 ()))) (:: 1 (:: 0 (:: 0 ()))) (:: 1 (:: 1 (:: 1 ()))) (:: 1 (:: 1 (:: 0 ()))) (:: 0 (:: 0 (:: 1 ()))) (:: 0 (:: 0 (:: 0 ()))) (:: 0 (:: 1 (:: 1 ()))) (:: 0 (:: 1 (:: 0 ()))) )  ) 



;  !(assertEqualToResult ( )  (  )  ) 
