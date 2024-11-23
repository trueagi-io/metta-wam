
; 20241014 r.v.v converted metta-lang.dev to metta 


(Parent Tom Bob)
(Parent Pam Bob)
(Parent Tom Liz)
(Parent Bob Ann)

; ! (match &self (Parent $x Bob) $x) 
; [Tom, Pam]


!(println "xx 46 ") 
; ready works 
!(assertEqualToResult ( match &self (Parent $x Bob) $x ) ( Tom Pam ) ) 

;[Pam, Tom]

