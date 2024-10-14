
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (== (A B) (A (B C))) ; False! (== 4 (+ Q 2)) ; False
(: R CustomType)
! (== 4 R) ; BadType

;  !(assertEqualToResult ( )  (  )  ) 
