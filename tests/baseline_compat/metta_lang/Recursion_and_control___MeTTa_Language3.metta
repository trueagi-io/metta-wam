
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 



(= (map $f ()) ())
(= (map $f (:: $x $xs))
   (:: ($f $x) (map $f $xs)))
(= (square $x) (* $x $x))
(= (twice $x) (* $x 2))

;! (map square (:: 1 (:: 2 (:: 3 ())))) 
; (:: 1 (:: 4 (:: 9 ())))
!(println "xx 64 ") 
 ; ready works 
!(assertEqualToResult ( map square (:: 1 (:: 2 (:: 3 ()))) ) ( (:: 1 (:: 4 (:: 9 ()))) ) ) 




;! (map twice (:: 1 (:: 2 (:: 3 ())))) 
; (:: 2 (:: 4 (:: 6 ())))
!(println "xx 65 ") 
 ; ready works  
!(assertEqualToResult ( map twice (:: 1 (:: 2 (:: 3 ()))) ) ( (:: 2 (:: 4 (:: 6 ()))) ) ) 


;! (map A (:: 1 (:: 2 (:: 3 ())))) 
; (:: (A 1) (:: (A 2) (:: (A 3) ())))
!(println "xx 66 ") 
 ; ready works  
!(assertEqualToResult ( map A (:: 1 (:: 2 (:: 3 ()))) ) ( (:: (A 1) (:: (A 2) (:: (A 3) ()))) ) ) 


;  !(assertEqualToResult ( )  (  )  ) 
