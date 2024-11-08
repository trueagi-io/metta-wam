
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;! (get-type B) 

;!(assertEqualToResult ( get-type B )  ( %Undefined% )  ) 
; 

;(: a A)
;!(assertEqualToResult ( get-type B )  (  )  ) 
;
;(: b B)
;! (get-type (a b)) 
;!(assertEqualToResult ( get-type (a b) )  ( A B )  ) 
; (A B)

;  !(assertEqualToResult ( )  (  )  ) 


(: a A)
(: b B)
; ! (get-type (a b)) 
; notready
!(assertEqualToResult ( get-type (a b) )  ( A B )  ) 
; (A B)