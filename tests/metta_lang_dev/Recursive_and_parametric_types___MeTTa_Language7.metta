
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(: Nil (List $t))
(: Cons (-&gt; $t (List $t) (List $t)))
! (get-type (Cons 1 (Cons 2 Nil)))
! (get-type (Cons False (Cons True Nil)))
! (get-type (Cons + (Cons - Nil)))

;  !(assertEqualToResult ( )  (  )  ) 
