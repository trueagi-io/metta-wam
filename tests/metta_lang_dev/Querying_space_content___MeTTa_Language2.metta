
; 20241014 r.v.v converted metta-lang.dev to metta 


(Parent Bob Ann)
; This match will be successful

;! (match &self (Parent Bob Ann) (Bob is Ann`s father))
; [(Bob is Ann`s father)]


!(println "xx 44 ") 
; ready works 
!(assertEqualToResult ( match &self (Parent Bob Ann) (Bob is Ann`s father) ) ( (Bob is Ann`s father) ) ) 



;! (match &self (Parent Bob Joe) (Bob is Joe`s father))
; []
!(println "xx 45 ") 
; ready works 
!(assertEqualToResult ( match &self (Parent Bob Joe) (Bob is Joe`s father) ) ( ) ) 



