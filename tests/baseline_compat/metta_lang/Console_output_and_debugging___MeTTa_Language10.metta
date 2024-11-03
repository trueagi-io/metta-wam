
; 20241014 r.v.v converted metta-lang.dev to metta 

(Parent Bob Ann)

;! (assertEqual
;   (match &self (Parent $x Ann) $x)
;   (unify (Parent $x Ann) (Parent Bob $y) $x )
;
;   ) 
;(match &self (Parent $x Ann) $x)
;(unify (Parent $x Ann) (Parent Bob $y) $x )
; [Failed]


!(println "xx 0 ") 
; notready 
!(assertEqualToResult ( match &self (Parent $x Ann) $x (unify (Parent $x Ann) (Parent Bob $y) $x ) ) ( Failed ) ) 


