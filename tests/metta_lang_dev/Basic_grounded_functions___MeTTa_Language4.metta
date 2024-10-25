
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;! (+ 2) ; BadType; Less than
;! (&lt; 1 3)
; Greater than
;! (&gt; 3 2)
; Less than or equal to
;! (&lt;= 5 6.2)
; Greater than or equal to
;! (&gt;= 4 (+ 2 (* 3 5)))


; Less than
;! (< 1 3)
; ready works
!(assertEqualToResult ( < 1 3 )  ( True )  ) 

; Greater than
;! (> 3 2)
; ready works
!(assertEqualToResult ( > 3 2 )  ( True )  ) 

; Less than or equal to
;! (<= 5 6.2)
; ready works
!(assertEqualToResult ( <= 5 6.2 )  ( True )  ) 

; Greater than or equal to
;! (>= 4 (+ 2 (* 3 5)))
; notready
!(assertEqualToResult ( >= 4 (+ 2 (* 3 5)) )  ( False )  ) 

;  !(assertEqualToResult ( )  (  )  ) 
