
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(= (call-by-value $f $arg)
   ($f $arg))


;! (call-by-value quote (+ 1 2)) 

; (quote 3)



; ready works 
!(assertEqualToResult ( call-by-value quote (+ 1 2) )  ( (quote 3) )  ) 
