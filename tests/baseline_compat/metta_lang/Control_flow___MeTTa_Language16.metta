
; 20241014 r.v.v converted metta-lang.dev to metta 


;! (case (empty)
;    ((Empty "Input was really empty")
;    ($_   "Should not be the case"))
;  )
;["Input was really empty"]


!(println "xx 23 ") 
; ready works 
!(assertEqualToResult ( case (empty) ((Empty "Input was really empty") ($_ "Should not be the case")) ) ( "Input was really empty" ) ) 


(= (rps-winner $x $y)
   (case ($x $y)
     (((Paper Rock) First)
      ((Scissors Paper) First)
      ((Rock Scissors) First)
      (($a $a) Draw)
      ($_ Second))
   )
)

;! (rps-winner Paper Scissors) 
; [Second]

!(println "xx 24 ") 
; ready works 
!(assertEqualToResult ( rps-winner Paper Scissors ) ( Second ) ) 

;! (rps-winner Rock  Scissors) 
; [First]

!(println "xx 25 ") 
; ready works  
!(assertEqualToResult ( rps-winner Rock Scissors ) ( First ) ) 


;! (rps-winner Paper Paper   ) 
; [Draw]

!(println "xx 26 ") 
; ready works 
!(assertEqualToResult ( rps-winner Paper Paper ) ( Draw ) ) 



