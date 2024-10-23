
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;! ((brother $x) is the brother of $x) ; the binding for $x is not lost

(= (square $x) (* $x $x))

; ready works
!(assertEqualToResult ( square 3 )  ( 9 )  ) 
