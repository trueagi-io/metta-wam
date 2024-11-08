
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(Parent Tom Bob)
(Parent Pam Bob)
(Parent Tom Liz)
(Parent Bob Ann)
(= (get-parent-entries $x $y)
   (match &self (Parent $x $y) (Parent $x $y)))
(= (get-parents $x)
   (match &self (Parent $y $x) $y))


;! (get-parent-entries Tom $_)
;[(Parent Tom Bob), (Parent Tom Liz)]


!(println "xx 0 ") 
; ready works 
!(assertEqualToResult ( get-parent-entries Tom $_ ) ( (Parent Tom Bob) (Parent Tom Liz) ) ) 


;! (get-parents Bob)
;[Pam, Tom]


!(println "xx 1 ") 
; ready works  
!(assertEqualToResult ( get-parents Bob ) ( Pam Tom ) ) 



;  !(assertEqualToResult ( )  (  )  ) 
