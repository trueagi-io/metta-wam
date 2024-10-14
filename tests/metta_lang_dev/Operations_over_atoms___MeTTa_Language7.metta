
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (cdr (Cons 1 (Cons 2 (Cons 3 Nil))))(: map (-&gt; (-&gt; $t $t) Expression Expression))
(= (map $f Nil) Nil)
(= (map $f (Cons $x $xs))
   (Cons ($f $x) (map $f $xs)))
! (map not (Cons False (Cons True (Cons False (Cons False Nil)))))

;  !(assertEqualToResult ( )  (  )  ) 
