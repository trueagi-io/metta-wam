
; 20241014 r.v.v converted metta-lang.dev to metta 

(= (is-frog Sam) True)
(= (print-if-frog $x)
   (let True (is-frog $x)
     (println! ($x is frog!))))

;! (print-if-frog Sam) 
; ()


!(println "xx 13 ") 
; ready works  
!(assertEqualToResult ( print-if-frog Sam ) ( () ) ) 



;! (print-if-frog Ben) 
; empty


!(println "xx 14 ") 
; ready works 
!(assertEqualToResult ( print-if-frog Ben ) ( ) ) 


