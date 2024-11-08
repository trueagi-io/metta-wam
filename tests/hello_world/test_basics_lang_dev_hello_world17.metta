


(= (length ...) 0)
(= (length ...)
   (+ 1 (length $xs)))
; ! (length (Cons A (Cons B (Cons C Nil))))

; website gives 
; [(length (Cons A (Cons B (Cons C Nil))))]

; mettalog gives [3]


!(assertEqualToResult ( length (Cons A (Cons B (Cons C Nil))) )  ( 3 )  ) 

