
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (&gt; $x (+ 8 2)) ; Inner expression is reduced, but the outer is not
! (&gt;= 4 (+ Q 2)) ; Reduction stops in the inner expression
(: R CustomType)
! (&gt;= 4 R) ; BadType

;  !(assertEqualToResult ( )  (  )  ) 
