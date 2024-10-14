
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (get-type (Cons True (Cons 1 Nil)))! (unify (-&gt; $t     (List $t) (List $t))
         (-&gt; Number (List $t) $result)
         $result
         BadType)

;  !(assertEqualToResult ( )  (  )  ) 
