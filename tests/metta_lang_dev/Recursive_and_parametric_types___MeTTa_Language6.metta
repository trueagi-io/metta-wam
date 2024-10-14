
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(: apply-twice (-&gt; ? ? ?))
(= (apply-twice $f $x)
   ($f ($f $x)))
(: greater-than-0 (-&gt; Number Bool))
(= (greater-than-0 $x) (&gt; $x 0))
! (get-type (apply-twice not True)) ; should be [Bool]
! (get-type (apply-twice greater-than-0 1)) ; should be []

;  !(assertEqualToResult ( )  (  )  ) 
