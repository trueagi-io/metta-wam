
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(= (get-hello) &hello)

; ! (bind! &hello (Hello world)) 
; [()]


!(println "xx 118 ") 
; ready works 
!(assertEqualToResult ( bind! &hello (Hello world) ) ( () ) ) 

;! (get-metatype &hello) 
; [Expression]


!(println "xx 119 ") 
; ready works  
!(assertEqualToResult ( get-metatype &hello ) ( Expression ) ) 

;!  
; [(Hello world)]
; notready
!(assertEqualToResult ( &hello )  ( (Hello world) )  ) 


;! () 
; [&hello]

; notready
!(assertEqualToResult ( get-hello )  ( &hello )  ) 

;[()]
;[Expression]
;[(Hello world)]
;[&hello]

;  !(assertEqualToResult ( )  (  )  ) 
