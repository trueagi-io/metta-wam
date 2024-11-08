
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;! (get-type match)


(Green Sam)
(= (Green Tod) True)

;! ($who (Green $who)) 
; (Tod True)
; ready works
!(assertEqualToResult ( $who (Green $who) )  ( (Tod True) )  ) 

;! (match &self (Green $who) $who) 
; ready works
!(assertEqualToResult ( match &self (Green $who) $who )  ( Sam )  ) 

; 



;  !(assertEqualToResult ( )  (  )  ) 
