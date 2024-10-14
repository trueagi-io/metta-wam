
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(: seq-and (-&gt; ... ... Bool))
(= (seq-and ... ...) ...)
(= (seq-and ... ...) ...)
(: loop (-&gt; Bool Bool))
! (seq-and False (loop)) ; should be False

;  !(assertEqualToResult ( )  (  )  ) 
