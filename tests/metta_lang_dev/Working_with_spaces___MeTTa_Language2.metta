
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


! (add-reduct &self (= (foo) (+ 3 4))) 
; ()
!(println "xx 101 ") 
; ready works notready 
;!(assertEqualToResult ( add-reduct &self (= (foo) (+ 3 4)) ) ( () ) ) 



;! (foo) 
; 7
!(println "xx 102 ") 
; ready works notready 
!(assertEqualToResult ( foo ) ( 7 ) ) 



;! (match &self (= (foo) $x)    (quote $x)) 
; (quote 7)
!(println "xx 103 ") 
; ready works notready 
!(assertEqualToResult ( match &self (= (foo) $x) (quote $x) ) ( (quote 7) ) ) 




;  !(assertEqualToResult ( )  (  )  ) 
