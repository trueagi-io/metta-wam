
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 

(= (reconstruct $xs)
   (let* (($head (car-atom $xs))
          ($tail (cdr-atom $xs)))
     (cons-atom $head $tail))
)

;! (reconstruct (1 2 3)) 
; (1 2 3)

!(println "xx 52 ") 
; ready works 
!(assertEqualToResult ( reconstruct (1 2 3) ) ( (1 2 3) ) ) 



;! (reconstruct (Cons X Nil)) 
; (Cons X Nil)

!(println "xx 53 ") 
; ready works  
!(assertEqualToResult ( reconstruct (Cons X Nil) ) ( (Cons X Nil) ) ) 




;  !(assertEqualToResult ( )  (  )  ) 
