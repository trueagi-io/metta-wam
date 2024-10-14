
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (reconstruct (Cons X Nil)) ; (Cons X Nil)! (cons-atom 1 (cdr-atom (1 2 3))) ; (1 cdr-atom (1 2 3))(: map-expr (-&gt; (-&gt; $t $t) Expression Expression))
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
! (map-expr not (False True False False))

;  !(assertEqualToResult ( )  (  )  ) 
