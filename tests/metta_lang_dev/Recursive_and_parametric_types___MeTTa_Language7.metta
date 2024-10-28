
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(: Nil (List $t))
(: Cons (-> $t (List $t) (List $t)))

;! (get-type (Cons 1 (Cons 2 Nil)))
;[(List Number)]

!(println "xx 89 ") 
 ; ready works 
!(assertEqualToResult ( get-type (Cons 1 (Cons 2 Nil)))  ( (List Number ) ) ) 


;! (get-type (Cons False (Cons True Nil)))
;[(List Bool)]

!(println "xx 90 ") 
 ; ready works  
!(assertEqualToResult ( get-type (Cons False (Cons True Nil)))  ( (List Bool ) ) ) 

;! (get-type (Cons + (Cons - Nil)))
;[(List (-> Number Number Number))]

!(println "xx 91 ") 
 ; ready works  
!(assertEqualToResult ( get-type (Cons + (Cons - Nil)))  ( (List (-> Number Number Number) ) ) ) 


;! (get-type (Cons True (Cons 1 Nil)))
;[]

!(println "xx 92 ") 
 ; ready works  
!(assertEqualToResult ( get-type (Cons True (Cons 1 Nil)) ) ( ) ) 


;  !(assertEqualToResult ( )  (  )  ) 
