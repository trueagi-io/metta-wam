
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
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
! (to-tree (False (True False) False False))

;  !(assertEqualToResult ( )  (  )  ) 
