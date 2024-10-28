
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 



(: NilNum ListNum)
(: ConsNum (-> Number ListNum ListNum))

;! (get-type (ConsNum 1 (ConsNum 2 (ConsNum 3 NilNum)))) 
; ListNum

!(println "xx 79 ") 
 ; ready works 
!(assertEqualToResult ( get-type (ConsNum 1 (ConsNum 2 (ConsNum 3 NilNum))) ) ( ListNum ) ) 


;! (ConsNum 1 (ConsNum "S" NilNum)) 
; BadType

!(println "xx 80 ") 
 ;  notready 
!(assertEqualToResult ( ConsNum 1 (ConsNum "S" NilNum) ) ( BadType ) ) 




;  !(assertEqualToResult ( )  (  )  ) 
