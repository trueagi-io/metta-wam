
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(: apply-twice (-> ? ? ?))
(= (apply-twice $f $x)
   ($f ($f $x)))
(: greater-than-0 (-> Number Bool))
(= (greater-than-0 $x) (> $x 0))


;! (get-type (apply-twice not True)) 
;  [Bool]

!(println "xx 87 ") 
 ; notready 
!(assertEqualToResult ( get-type (apply-twice not True) ) ( Bool ) ) 


;! (get-type (apply-twice greater-than-0 1)) 
;  []

!(println "xx 88 ") 
 ; ready works 
!(assertEqualToResult ( get-type (apply-twice greater-than-0 1) ) ( ) ) 




;  !(assertEqualToResult ( )  (  )  ) 
