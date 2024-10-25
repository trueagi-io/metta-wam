
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(: a A)

;! (get-type (+ 1 a)) 
; empty
; ready works 
!(assertEqualToResult ( get-type (+ 1 a))  (  )  )

;! (get-type (+ 1 b)) 
; Number
; notready
!(assertEqualToResult ( get-type (+ 1 b) )  ( Number )  )

;! (+ 1 b) 
; no error, not reduced
; readyworks 
!(assertEqualToResult ( (+ 1 b) )  ( (+ 1 b) )  )


;! (+ 1 a) 
; type error
; readyworks 
!(assertEqualToResult ( (+ 1 a) )  ( (+ 1 a) )  )



;  !(assertEqualToResult ( )  (  )  ) 
