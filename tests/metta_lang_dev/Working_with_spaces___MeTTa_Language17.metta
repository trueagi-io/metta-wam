
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (mod-space! stdlib)
! (match (mod-space! stdlib)
          (= (if $cond $then $else) $result)
   (quote (= (if $cond $then $else) $result))
  )

;  !(assertEqualToResult ( )  (  )  ) 
