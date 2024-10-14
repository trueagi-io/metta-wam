
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(: apply (-&gt; (-&gt; $tx $ty) $tx $ty))
(= (apply $f $x) ($f $x))
! (apply not False) ; True
! (get-type (apply not False)) ; Bool
! (unify (-&gt; (-&gt; $tx  $ty)  $tx  $ty)
         (-&gt; (-&gt; Bool Bool) Bool $result)
         $result
         BadType) ; Bool
! (apply not 1) ; BadType

;  !(assertEqualToResult ( )  (  )  ) 
