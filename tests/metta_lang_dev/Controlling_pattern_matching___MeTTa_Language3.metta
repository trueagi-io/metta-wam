
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;! (match &amp

;self (Green $who) $who) 
; Sam(Green Sam)

;(Green Tod)
;(= (Frog Sam) True)

(Green Sam)
(= (Frog Sam) True)
; ! (match &self (Green $who) (quote (Frog $who)))

; ready works
!(assertEqualToResult ( match &self (Green $who) (quote (Frog $who)) )  ( (quote (Frog Sam)) )  ) 




;  !(assertEqualToResult ( )  (  )  ) 
