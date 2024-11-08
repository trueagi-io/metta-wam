
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


;! (get-type Error) 
; (-> Atom Atom ErrorType)
!(println "xx 31 ") 
 ; ready works 
!(assertEqualToResult ( get-type Error ) ( (-> Atom Atom ErrorType) ) ) 



;! (get-metatype Error) 
; just Symbol
!(println "xx 32 ") 
 ; notready 
!(assertEqualToResult ( get-metatype Error ) ( Symbol ) ) 


;! (get-type (Error Foo Boo)) 
; ErrorType
!(println "xx 33 ") 
 ;  notready 
!(assertEqualToResult ( get-type (Error Foo Boo) ) ( ErrorType ) ) 


;! (Error (+ 1 2) (+ 1 +)) 
; arguments are not evaluated
!(println "xx 34 ") 
 ; ready works notready 
!(assertEqualToResult ( Error (+ 1 2) (+ 1 +) ) ( ( Error (+ 1 2) (+ 1 +)) ) ) 




;  !(assertEqualToResult ( )  (  )  ) 
