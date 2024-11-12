
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(: apply (-> (-> $tx $ty) $tx $ty))
(= (apply $f $x) ($f $x))

;!(apply not False) 
; True
!(println "xx 83 ") 
 ; ready works 
!(assertEqualToResult ( apply not False ) ( True ) ) 


;!(get-type (apply not False)) 
; Bool
!(println "xx 84 ") 
 ; ready works 
!(assertEqualToResult ( get-type (apply not False) ) ( Bool ) ) 


;!(unify (-> (-> $tx  $ty)  $tx  $ty)
;         (-> (-> Bool Bool) Bool $result)
;         $result
;         BadType) 
 ; Bool
!(println "xx 85 ") 
 ;  notready 
!(assertEqualToResult ( unify (-> (-> $tx $ty) $tx $ty) (-> (-> Bool Bool) Bool $result) $result BadType ) ( Bool ) ) 


;! (apply not 1) 
; BadType
!(println "xx 86 ") 
 ; notready 
!(assertEqualToResult ( apply not 1 ) ( BadType ) ) 



;  !(assertEqualToResult ( )  (  )  ) 
