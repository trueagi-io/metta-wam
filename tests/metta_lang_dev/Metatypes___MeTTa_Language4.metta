
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(: my-if (-> Bool Atom Atom Atom))
(= (my-if True $then $else) $then)
(= (my-if False $then $else) $else)
(= (loop) (loop))
(= (OK) OK!)

;! (my-if (> 0 1) (loop) (OK))
; [OK!]


!(println "xx 38 ") 
 ; ready works 
!(assertEqualToResult ( my-if (> 0 1) (loop) (OK) ) ( OK! ) ) 


;  !(assertEqualToResult ( )  (  )  ) 
