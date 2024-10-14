
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (cons-atom seg ((point 1 1) (point 1 4))) ; (seg (point 1 1) (point 1 4))(= (reconstruct $xs)
   (let* (($head (car-atom $xs))
          ($tail (cdr-atom $xs)))
     (cons-atom $head $tail))
)
! (reconstruct (1 2 3)) ; (1 2 3)

;  !(assertEqualToResult ( )  (  )  ) 
