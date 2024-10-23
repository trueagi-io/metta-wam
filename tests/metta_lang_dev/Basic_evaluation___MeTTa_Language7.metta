
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
; ! (swap (Pair A B)) 
; evaluates to (Pair B A)(= (Cdr (Cons $x $xs)) $xs)
; ! (Cdr (Cons A (Cons B Nil))) 
(= (Cdr (Cons $x $xs)) $xs)
; outputs (Cons B Nil)

; ready works
!(assertEqualToResult ( Cdr (Cons A (Cons B Nil)) )  ( (Cons B Nil) )  ) 
