
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 

;  !(assertEqualToResult ( )  (  )  ) 


(: foo (-> (-> A B) C))
(: bar (-> A B))
(: a A)

;! (get-type (foo bar)) 
; C
; notready 
!(assertEqualToResult ( get-type (foo bar) )  ( C )  ) 

;! (get-type (foo (bar a))) 
; ready works 
!(assertEqualToResult ( get-type (foo (bar a)) )  (  )  ) 

; empty