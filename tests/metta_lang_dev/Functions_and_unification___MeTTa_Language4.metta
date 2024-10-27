
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(Parent Tom Bob)
(Parent Bob Ann)
(Implies (Parent $x $y) (Child $y $x))
(= (deduce $B)
   (match &self (Implies $A $B)
          (match &self $A $B))
)
(= (conclude $A)
   (match &self (Implies $A $B)
          (match &self $A $B))
)


;! (deduce (Child $x Tom))    
; [(Child Bob Tom)]

!(println "xx 2 ") 
 ; ready works 
!(assertEqualToResult ( deduce (Child $x Tom) ) ( (Child Bob Tom) ) ) 


;! (conclude (Parent Bob $y)) 
; [(Child Ann Bob)]

!(println "xx 3 ") 
 ; ready works  
!(assertEqualToResult ( conclude (Parent Bob $y) ) ( (Child Ann Bob) ) ) 



;  !(assertEqualToResult ( )  (  )  ) 
