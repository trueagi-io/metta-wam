
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;(Green Sam)
;(= (Green Tod) True)
;! ($who (Green $who)) ; (Tod True)


(Green Sam)
(Green Tod)
(= (Frog Sam) True)
;! (match &self (Green $who) (Frog $who)) 
; ready works 
!(assertEqualToResult ( match &self (Green $who) (Frog $who) )  (  True (Frog Tod) )  ) 

; [True, (Frog Tod)]



;  !(assertEqualToResult ( )  (  )  ) 
