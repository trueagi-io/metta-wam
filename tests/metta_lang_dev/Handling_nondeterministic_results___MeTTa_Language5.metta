
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 

(= (eq $x $x) True)
;! (eq a b) 
; (eq a b)


!(println "xx 13 ") 
 ; ready works  
!(assertEqualToResult ( eq a b ) ( (eq a b) ) ) 



(= (eq $x $y) (empty))

;! (eq a b) 
; no result


!(println "xx 14 ") 
 ; ready works  
!(assertEqualToResult ( eq a b ) () ) 


;  !(assertEqualToResult ( )  (  )  ) 
