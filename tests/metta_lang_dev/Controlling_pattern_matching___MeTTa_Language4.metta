
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;! (match &amp;self (Green $who) (Frog $who)) ; [True, (Frog Tod)](Green Sam)
;(= (Frog Sam) True)
;! (match &amp;self (Green $who) (quote (Frog $who)))


(Green Sam)
(Likes Sam Emi)
(Likes Tod Kat)

; ready works
!(assertEqualToResult ( match &self (Green $who)    (match &self (Likes $who $x) $x) )  ( Emi )  ) 
;! ()

;! ()
; ready works
!(assertEqualToResult ( match &self (Green $who)    (match &self (Likes $boo $x) $x))  ( Kat Emi )  ) 

;! ()
; ready works
!(assertEqualToResult ( match &self (Likes $who $x)    (match &self (Green $x) $x))  (  )  ) 

;! ()
; ready works
!(assertEqualToResult ( match &self (Likes $who $x)    (match &self (Green $boo) $boo))  (  Sam Sam )  ) 


;[Emi]
;[Kat, Emi]
;[]
;[Sam, Sam]

;  !(assertEqualToResult ( )  (  )  ) 
