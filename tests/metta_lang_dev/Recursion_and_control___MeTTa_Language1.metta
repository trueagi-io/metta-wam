
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(= (length ()) 0)
(= (length (:: $x $xs))
   (+ 1 (length $xs)))


;! (length (:: A (:: B (:: C ()))))
; [3]



; ready works 
!(assertEqualToResult ( length (:: A (:: B (:: C ()))) )  ( 3 )  ) 
