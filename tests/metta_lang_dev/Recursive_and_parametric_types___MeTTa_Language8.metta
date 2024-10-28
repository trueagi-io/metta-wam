
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


;! (unify (-> $t     (List $t) (List $t))
;         (-> Number (List $t) $result)
;         $result
;         BadType)

; [(List Number)]



!(println "xx 93 ") 
 ;  notready 
!(assertEqualToResult ( unify (-> $t (List $t) (List $t)) (-> Number (List $t) $result) $result BadType ) ( (List Number) ) ) 

;  !(assertEqualToResult ( )  (  )  ) 
