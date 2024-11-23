
; 20241014 r.v.v converted metta-lang.dev to metta 


(= (zip () ()) ())
(= (zip (:: $x $xs) (:: $y $ys))
   (:: ($x $y) (zip $xs $ys)))

;! (zip (:: A (:: B ())) (:: 1 (:: 2 ()))) ; (:: (A 1) (:: (B 2) ()))
; [(:: (A 1) (:: (B 2) ()))]


!(println "xx 49 ") 
; ready works  
!(assertEqualToResult ( zip (:: A (:: B ())) (:: 1 (:: 2 ())) ) ( (:: (A 1) (:: (B 2) ())) ) ) 



;! (zip (:: A (:: B ())) (:: 1 ())) ; (:: (A 1) (zip (:: B ()) ()))
; [(:: (A 1) (zip (:: B ()) ()))]


!(println "xx 50 ") 
; ready works 
!(assertEqualToResult ( zip (:: A (:: B ())) (:: 1 ()) ) ( (:: (A 1) (zip (:: B ()) ())) ) ) 
