
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


;! (match &self
;          (= (if $cond $then $else) $result)
;   (quote (= (if $cond $then $else) $result))
;  )
;[(quote (= (if True $then $else) $then)), (quote (= (if False $then $else) $else))]



!(println "xx 131 ") 
; notready 
!(assertEqualToResult ( match &self (= (if $cond $then $else) $result) (quote (= (if $cond $then $else) $result)) ) ( (quote (= (if True $then $else) $then)) (quote (= (if False $then $else) $else)) ) ) 



;  !(assertEqualToResult ( )  (  )  ) 
