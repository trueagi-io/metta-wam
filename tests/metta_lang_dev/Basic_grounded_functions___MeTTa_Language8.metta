
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
; Test if both the given expressions are True
! (and (&gt; 4 2) (==)) ; False
; Test if any of the given expressions is True
! (or (&gt; 4 2) (==)) ; True
; Negates the result of a given Bool value
! (not (== 5 5)) ; False
! (not (and (&gt; 4 2) (&lt; 4 3))) ; True

;  !(assertEqualToResult ( )  (  )  ) 
