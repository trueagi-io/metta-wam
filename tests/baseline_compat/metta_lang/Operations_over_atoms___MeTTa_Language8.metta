
; 20241014 r.v.v converted metta-lang.dev to metta 

(= (to-list $expr)
   (if (== $expr ()) Nil
     (Cons (car-atom $expr)
           (to-list (cdr-atom $expr)))
   )
)

;! (to-list (False (True False) False False))
; [(Cons False (Cons (True False) (Cons False (Cons False Nil))))]


!(println "xx 42 ") 
 ; ready works 
!(assertEqualToResult ( to-list (False (True False) False False) ) ( (Cons False (Cons (True False) (Cons False (Cons False Nil)))) ) ) 