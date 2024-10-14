
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(: Nil (List $t))
(: Cons (-&gt; $t (List $t) (List $t)))
(: first (-&gt; (List $t) $t))
(: append (-&gt; (List $t) (List $t) (List $t)))
! (get-type
    (+ 1
      (first (append (Cons 1 Nil)
                     (Cons 2 Nil)))))

;  !(assertEqualToResult ( )  (  )  ) 
