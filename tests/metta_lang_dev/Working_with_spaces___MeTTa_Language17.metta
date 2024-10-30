
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


! (mod-space! stdlib)
;[GroundingSpace-top:stdlib]


;! (match (mod-space! stdlib)
;          (= (if $cond $then $else) $result)
;   (quote (= (if $cond $then $else) $result))
;  )
;[(quote (= (if False $then $else) $else)), (quote (= (if True $then $else) $then))]



!(println "xx 132 ") 
; notready 
!(assertEqualToResult ( match (mod-space! stdlib) (= (if $cond $then $else) $result) (quote (= (if $cond $then $else) $result)) ) ( (quote (= (if False $then $else) $else)) (quote (= (if True $then $else) $then)) ) ) 


;  !(assertEqualToResult ( )  (  )  ) 
