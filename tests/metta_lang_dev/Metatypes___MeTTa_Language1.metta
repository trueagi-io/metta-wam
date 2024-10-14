
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (get-metatype 1) ; Grounded
! (get-metatype +) ; Grounded
! (get-metatype (+ 1 2)) ; Expression
! (get-metatype a) ; Symbol
! (get-metatype (a b)) ; Expression
! (get-metatype $x) ; Variable

;  !(assertEqualToResult ( )  (  )  ) 
