
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;! (&gt; $x (+ 8 2)) ; Inner expression is reduced, but the outer is not
;! (&gt;= 4 (+ Q 2)) ; Reduction stops in the inner expression
;(: R CustomType)
;! (&gt;= 4 R) ; BadType

;! (> $x (+ 8 2)) 
; ready works 
!(assertEqualToResult ( > $x (+ 8 2) )  ( (> $x 10) )  ) 

; Inner expression is reduced, but the outer is not
;! (>= 4 (+ Q 2)) 
;!(assertEqualToResult ( >= 4 (+ Q 2) )  ( (>= 4 (+ Q 2)) )  ) 
; ready works 
!(assertEqualToResult ( > 4 (+ Q 2) )  ( (> 4 (+ Q 2)) )  ) 
; Reduction stops in the inner expression
;(: R CustomType)
;(= R (3) )

;!(assertEqualToResult (: R CustomType )  (  )  ) 
; ! (>= 4 R) 
;!(assertEqualToResult ( >= 4 R )  ( (>= 4 R) )  ) 
;!(assertEqualToResult ( >= 4 R )  ( True )  ) 

; notready 
;(= (R) 3 )
;!(assertEqualToResult (> 4 R )  ( True )  ) 

; ready works 
!(assertEqualToResult (> 4 R )  ( True )  ) 
; BadType

;[(> $x 10)]
;[(>= 4 (+ Q 2))]
;[(Error R BadType)]

;  !(assertEqualToResult ( )  (  )  ) 
