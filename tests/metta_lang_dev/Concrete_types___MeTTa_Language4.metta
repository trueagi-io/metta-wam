
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;(= ($1 infix-f $2) ($2 $1))
;! (match &amp;self (= (1 infix-f 2) $r) $r)

(= ($1 infix-f $2) ($2 $1))
;! (match &self (= (1 infix-f 2) $r) $r)

; ready works 
!(assertEqualToResult ( match &self (= (1 infix-f 2) $r) $r )  ( (2 1) )  ) 
