
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



;! (unify (parent $x Bob) 
;         (parent Tom $y) 
;         ($x $y) 
;         Fail) 
;[(Tom Bob)]


!(println "xx 4 ") 
 ; notready 
!(assertEqualToResult ( unify (parent $x Bob) (parent Tom $y) ($x $y) Fail ) ( (Tom Bob) ) ) 


;!(assertEqualToResult ( deduce (Child $x Tom) )  ( (Child Bob Tom) )  ) 

;!(assertEqualToResult ( conclude (Parent Bob $y) )  ( (Child Ann Bob) )  ) 
