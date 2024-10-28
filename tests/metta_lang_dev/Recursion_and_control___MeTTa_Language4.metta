
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(= (factorial $x)
   (if (> $x 0)
       (* $x (factorial (- $x 1)))
       1))

;! (factorial 5) 
; 120


!(println "xx 67 ") 
 ; ready works 
!(assertEqualToResult ( factorial 5 ) ( 120 ) ) 


;  !(assertEqualToResult ( )  (  )  ) 
