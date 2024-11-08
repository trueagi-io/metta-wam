
; 20241014 r.v.v converted metta-lang.dev to metta 

(= (to-tree $expr)
   (case (get-metatype $expr)
      ((Expression
         (if (== $expr ()) Nil
             (Cons (to-tree (car-atom $expr))
                   (to-tree (cdr-atom $expr)))
         ))
       ($_ $expr)
      )
   )
)
;! (to-tree (False (True False) False False))
; [(Cons False (Cons (Cons True (Cons False Nil)) (Cons False (Cons False Nil))))]



!(println "xx 43 ") 
 ; ready works 
!(assertEqualToResult ( to-tree (False (True False) False False) ) ( (Cons False (Cons (Cons True (Cons False Nil)) (Cons False (Cons False Nil)))) ) ) 
