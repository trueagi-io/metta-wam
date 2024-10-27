
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(= (only-a A) (Input A is accepted))

;! (match &self (= (only-a A) $result) $result)
;[(Input A is accepted)]

!(println "xx2") 
 ; ready works 
!(assertEqualToResult ( match &self (= (only-a A) $result) $result ) ( (Input A is accepted) ) ) 



;! (match &self (= (only-a B) $result) $result)
;[]
!(println "xx3") 
 ; ready works  
!(assertEqualToResult ( match &self (= (only-a B) $result) $result ) ( ) ) 


;! (match &self (= (only-a $x) $result) $result)
;[(Input A is accepted)]
!(println "xx4") 
 ; ready works  
!(assertEqualToResult ( match &self (= (only-a $x) $result) $result ) ( (Input A is accepted) ) ) 








;  !(assertEqualToResult ( )  (  )  ) 
