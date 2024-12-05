
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


;! (get-type ==)
;[(-> $t#1168 $t#1168 Bool)]


!(println "xx 81 ") 
 ; notready 
!(assertEqualToResult ( get-type ==)  ( (-> $t#1168 $t#1168 Bool ) ) ) 


;! (== 1 "S")
;[(Error "S" BadType)]


!(println "xx 82 ") 
 ; notready 
!(assertEqualToResult ( == 1 "S")  ( (Error "S" BadType ) ) ) 



;  !(assertEqualToResult ( )  (  )  ) 
