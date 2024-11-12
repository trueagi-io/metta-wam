
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(= (test 1) 2)

;! (let 2 (test 1) YES) 
; YES
; ready works
!(assertEqualToResult ( let 2 (test 1) YES )  ( YES )  ) 

;! (let (test 1) 2 NO ) 
; empty
; ready works
!(assertEqualToResult ( let (test 1) 2 NO  )  (  )  ) 


;  !(assertEqualToResult ( )  (  )  ) 
