
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 

(= (ikb) international-klein-blue)
(= (color) green)
(= (color) yellow)
(= (color) (ikb))

;!(assertEqual
;  ((match &self (= (color) $x) $x)
;  (superpose ((ikb) yellow green)) )
 ;()

;!(println "xx 12 ") 

 ; ready works 
!(assertEqual
  (match &self (= (color) $x) $x)
  (superpose ((ikb) yellow green))) 

; ()

; !(println "xx 13 ") 

; notready 
!(assertEqualToResult
   (match &self (= (color) $x) $x)
  ((ikb) yellow green)) 

 ; Error



 ; ready works notready 
;!(assertEqualToResult ( (match &self (= (color) $x) $x) (superpose ((ikb) yellow green)) ) ( () ) ) 


;!(assertEqualToResult
;   (match &self (= (color) $x) $x)
;  ((ikb) yellow green) 
;   Error


;!(println "xx 11 ") 
 ; ready works notready 
;!(assertEqualToResult ( match &self (= (color) $x) $x) ((ikb) yellow green ) ( Error ) ) 




;  !(assertEqualToResult ( )  (  )  ) 
