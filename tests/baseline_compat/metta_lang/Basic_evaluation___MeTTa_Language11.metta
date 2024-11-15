
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(= (brother Mike) Tom)
(= (brother Sam) Bob)
;! (brother $x) 

; ready works
!(assertEqualToResult (brother $x)  ( Tom Bob )  ) 

; just Tom and Bob are returned

; ! ((brother $x) is the brother of $x)
; ready works
!(assertEqualToResult ( (brother $x) is the brother of $x )  (( Bob is the brother of Sam) (Tom is the brother of Mike )  ) ) 
