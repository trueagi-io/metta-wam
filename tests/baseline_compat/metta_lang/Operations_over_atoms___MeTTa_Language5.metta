
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(: map (-> (-> $t $t) Expression Expression))
(= (map $f Nil) Nil)
(= (map $f (Cons $x $xs))
   (Cons ($f $x) (map $f $xs)))


;! (map not (Cons False (Cons True (Cons False (Cons False Nil)))))
; [(Cons True (Cons False (Cons True (Cons True Nil))))]


!(println "xx 56 ") 
 ; ready works 
!(assertEqualToResult ( map not (Cons False (Cons True (Cons False (Cons False Nil)))) ) ( (Cons True (Cons False (Cons True (Cons True Nil)))) ) ) 

;  !(assertEqualToResult ( )  (  )  ) 
