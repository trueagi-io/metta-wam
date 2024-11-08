
; 20241014 r.v.v converted metta-lang.dev to metta 

(Parent Bob Ann)
(Parent Pam (Parent Bob Pat))

;! (match &self (Parent Bob $x) $x) 
; Ann


!(println "xx 47 ") 
; ready works 
!(assertEqualToResult ( match &self (Parent Bob $x) $x ) ( Ann ) ) 



;[Ann]

(Parent Tom Bob)
(Parent Pam Bob)
(Parent Tom Liz)
(Parent Bob Ann)
(Parent Bob Pat)
(Parent Pat Pat)

;! (match &self (Parent $x $y) ($x $y))
;[(Bob Pat), (Bob Ann), (Pat Pat), (Pam Bob), (Tom Bob), (Tom Liz)]


;((Error  
;	 (got  ((Tom Bob) (Pam Bob) (Tom Liz) (Bob Ann) (Pam (Parent Bob Pat)) (Bob Pat) (Pat Pat))) 
;	 (expected  ((Bob Pat) (Bob Ann) (Pat Pat) (Pam Bob) (Tom Bob) (Tom Liz)))))]



!(println "xx 48 ") 
;  notready 
!(assertEqualToResult ( match &self (Parent $x $y) ($x $y) ) ( (Bob Pat) (Bob Ann) (Pat Pat) (Pam Bob) (Tom Bob) (Tom Liz) ) ) 


