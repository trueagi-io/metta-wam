
; 20241014 r.v.v converted metta-lang.dev to metta 

(= (zip $list1 $list2)
   (case ($list1 $list2)
         (((() ()) ())
          (((:: $x $xs) (:: $y $ys)) (:: ($x $y) (zip $xs $ys)))
          ($else ERROR)
         )
   )
)


;! (zip (:: A (:: B ())) (:: 1 (:: 2 ()))) 
; (:: (A 1) (:: (B 2) ()))


!(println "xx 51 ") 
; ready works
!(assertEqualToResult ( zip (:: A (:: B ())) (:: 1 (:: 2 ())) ) ( (:: (A 1) (:: (B 2) ())) ) ) 


;! (zip (:: A (:: B ())) (:: 1 ())) 
; (:: (A 1) ERROR)


!(println "xx 52 ") 
; ready works 
!(assertEqualToResult ( zip (:: A (:: B ())) (:: 1 ()) ) ( (:: (A 1) ERROR) ) ) 



;[(:: (A 1) (:: (B 2) ()))]
;[(:: (A 1) ERROR)]


