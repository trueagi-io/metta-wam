
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(= (get-space) (new-space))


; ! (add-atom (get-space) (Parent Bob Ann)) 
; [()]


!(println "xx 111 ") 
; ready works 
!(assertEqualToResult ( add-atom (get-space) (Parent Bob Ann) ) ( () ) ) 


;! (match (get-space) (Parent $x $y) ($x $y)) 
; []


!(println "xx 112 ") 
; ready works 
!(assertEqualToResult ( match (get-space) (Parent $x $y) ($x $y) ) ( ) ) 



;[()]
;[]

;  !(assertEqualToResult ( )  (  )  ) 
