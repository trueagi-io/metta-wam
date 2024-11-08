
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(= (factorial $x)
   (case $x
     ((0 1)
      ($_ (* $x (factorial (- $x 1)))))
   )
)
; ! (factorial 5) 
; 120


!(println "xx 68 ") 
 ; ready works 
!(assertEqualToResult ( factorial 5 ) ( 120 ) ) 

;  !(assertEqualToResult ( )  (  )  ) 
