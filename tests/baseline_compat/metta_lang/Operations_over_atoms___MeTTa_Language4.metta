
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(: map-expr (-> (-> $t $t) Expression Expression))
(= (map-expr $f $expr)
   (if (== $expr ()) ()
       (let* (($head (car-atom $expr))
              ($tail (cdr-atom $expr))
              ($head-new ($f $head))
              ($tail-new (map-expr $f $tail))
             )
         (cons-atom $head-new $tail-new)
       )
   )
)



;! (map-expr not (False True False False))
;[(True False True True)]


!(println "xx 55 ") 
; ready works 
!(assertEqualToResult ( map-expr not (False True False False) ) ( (True False True True) ) ) 


;  !(assertEqualToResult ( )  (  )  ) 
