
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(Parent Tom Bob)
(Parent Bob Ann)

; ! (unify &self (Parent $x Bob) $x Fail) ; [Tom]
; [Tom]


!(println "xx 5 ") 
 ; ready works  
!(assertEqualToResult ( unify &self (Parent $x Bob) $x Fail ) ( Tom ) ) 


;  !(assertEqualToResult ( )  (  )  ) 
